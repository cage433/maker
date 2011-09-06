package starling.utils.sql

import org.springframework.jdbc.core.namedparam.{MapSqlParameterSource, NamedParameterUtils}
import starling.utils.{Log}
import starling.instrument.utils.StarlingXStream

case class SQL(val query:String, parameters:Map[String,Any])

class SqlRenderer {
  var sequence = 0
  def render(query: Query): SQL = {
    val (where, whereParams) = renderClause(query.where)
    val (having, havingParams) = renderHaving(query.havingClause)
    val (join, joinParams) = expandJoins(query.joins)
    val (tableText, tableParams) = expandTable(query.from.table)
    val sql =
      "select " + expandSelect(query.select)+ "\n" +
      "from " + tableText + "\n" +
      join +
      where +
      expandGroupBy(query.groupBy) +
      having +
      expandOrder(query)

    SQL(sql, whereParams ++ tableParams ++ joinParams ++ havingParams)
  }

  def render(modify: Modify) : SQL = {
    val (setText, setParams) = expandClause(modify.set)
    val (where, whereParams) = renderClause(modify.where)
    val (tableText, tableParams) = expandTable(modify.update.table)
    val sql =
      "update " + tableText + "\n" +
      "set " + setText + "\n" +
      where
    
    SQL(sql, setParams ++ whereParams ++ tableParams)
  }

  def expandSelect(select:Select):String = {
    def escape(field:String) = field
    select.fields.map(escape(_)).mkString(", ")
  }

  def expandJoins(joins:List[Join]):(String,Map[String,Any]) = {
    var allParams = Map[String, Any]()
    val s = joins.map((join)=>{
      val (strClause, params) = expandClause(join.clause)
      allParams = allParams ++ params
      val t = expandTable(join.table)
      allParams = allParams ++ t._2
      " " + join.joinType + " join " + t._1 + " on " + strClause + "\n"
    }
    ).mkString("")
    (s, allParams)
  }

  def expandTable(table:Table):(String,Map[String,Any]) = {
    table match {
      case t:RealTable => { (t.name, Map()) }
      case j:QueryTable => {
        val query = render(j.query)
        (" (" + query.query + ") " + j.alias + " ", query.parameters)
      }
    }
  }

  def expandField(field:Field) : String = field match {
    case f => f.name.toLowerCase match { case "type" => "\""+f.name+"\""; case _ => f.name }
  }

  private def param(field:Field) = {
    val s = sequence
    sequence = sequence + 1

    // we want to strip out any functions like LOWER(fieldName)
    val ParamWithFunction = """[A-Z]*\(([\w\._]+)\)""".r
    val fieldName = field.name.filterNot(_ == '\"')

    if(fieldName.matches(ParamWithFunction.pattern.pattern)) {
      val ParamWithFunction(realName) = field.name.filterNot(_ == '\"')
      realName + "_" + s
    } else {
      fieldName + "_" + s
    }
  }

  def operator(field:Field, operator:String, value:Any) : (String, Map[String, Any]) = {
    value match {
      // First case is for matching column names, e.g. ("c.id" eql "h.CalendarTypeID")
      // Will break if a String contains '.', as user names and RefinedFixation trade IDs do - if this could be
      // the case then use a LiteralString instead
      case s : String if(s.contains('.')) => {
          (expandField(field) + " " + operator + " " + value, Map())
      }
      case PersistAsBlob(obj) => val p = param(field); (expandField(field) + " " + operator + " " + ":"+p, Map(p -> StarlingXStream.write(obj)))
      case LiteralString(text) => val p = param(field); (expandField(field) + " " + operator + " " + ":"+p, Map(p -> text))
      case _ => val p = param(field); (expandField(field) + " " + operator + " " + ":"+p, Map(p -> value))
    }
  }

  def expandGroupBy(fields:List[String]) = {
    fields match {
      case Nil => ""
      case _ => "\ngroup by\n\t" + fields.mkString(", ")
    }
  }

  def renderClause(clause: Option[Clause]): (String, Map[String, Any]) = clause match {
    case None => ("", Map())
    case Some(clause) => {
      val (str, map) = expandClause(clause)
      ("where\n\t" + str, map)
    }
  }

  def renderHaving(clause: Option[Clause]): (String, Map[String, Any]) = clause match {
    case None => ("", Map())
    case Some(clause) => {
      val (str, map) = expandClause(clause)
      ("\nhaving \n\t" + str, map)
    }
  }

  def expandClause(clause: Clause): (String, Map[String, Any]) = clause match {
    case Equals(field, lazyVal) => operator(field, "=", lazyVal)
    case Like(field, pattern) => operator(field, "like", pattern)
    case NotLike(field, pattern) => operator(field, "not like", pattern)
    case GreaterThan(field, lazyVal) => operator(field, ">", lazyVal)
    case GreaterThanOrEqual(field, lazyVal) => operator(field, ">=", lazyVal)
    case LessThan(field, lazyVal) => operator(field, "<", lazyVal)
    case LessThanOrEqual(field, lazyVal) => operator(field, "<=", lazyVal)
    case NotEquals(field, lazyVal) => operator(field, "!=", lazyVal)
    case In(field, values) => if (values.isEmpty) ("1=2", Map()) else {
      val p = param(field)
      (expandField(field) + " in (:"+p+")", Map(p -> values))
    }
    case NotIn(field, value) => val p = param(field); (expandField(field) + " not in (:"+p+")", Map(p -> value))
    case Not(clause) => {
      val exp = expandClause(clause)
      ("!(%s)".format(exp._1), exp._2)
    }
    case NotNull(field) => (field.name + " is not null", Map())
    case IsNull(field) => (field.name + " is null", Map())
    case FalseClause => ("1=2", Map())
    case and:And => {
      val left = expandClause(and.lClause)
      val right = expandClause(and.rClause)
      ("(%s \n\tand %s)".format(left._1,right._1), addMapsSafely(left, right))
    }
    case or:Or => {
      val left = expandClause(or.lClause)
      val right = expandClause(or.rClause)
      ("(%s \n\tor %s)".format(left._1,right._1), addMapsSafely(left, right))
    }
    case _ => throw new IllegalArgumentException("Clause %s not implemented".format(clause))
  }

  /**
   * This method adds two maps of params together while making sure that they don't have duplicate keys. If they do
   * have duplicate keys it could cause trouble with a query. For example:
   * 'select * from T where a > :A and a < :A', Map('A' -> 2), Map('A' -> 4)
   * When these two param maps were added together this method would throw an exception.
   * This method will warn if there are duplicate params that have the same value.
   */
  def addMapsSafely(l: (String,Map[String, Any]), r: (String,Map[String, Any])) : Map[String, Any] = {
    val combined = l._2.keySet ++ r._2.keySet
    if(combined.size != l._2.size + r._2.size) {
      val dups = combined.filter(k => l._2.contains(k) && r._2.contains(k))
      if(dups.forall(k=>l._2(k) == r._2(k))) {
        Log.warn("Duplicate keys in query, this (possibly?) shouldn't be the case, although the values are the same. Keys: " + dups.mkString(", "))
      } else {
        throw new Exception("Invalid query. Duplicate keys: " + dups.mkString(", ") + " in queries " + l._1 + " with " + r._1)
      }
    }
    l._2 ++ r._2    
  }

  def expandOrder(query: Query) = query.order match {
    case Some(direction) => direction match {
      case Asc(field) => "\norder by %s asc".format(field)
      case Desc(field) => "\norder by %s desc".format(field)
    }
    case None => ""
  }

  def quote(value: String) = "'%s'".format(escape(value))
  def escape(value: String) = value.replaceAll("'", "''")
}
