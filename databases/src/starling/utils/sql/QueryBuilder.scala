package starling.utils.sql


case class Query(select:Select, from:From, where: Option[Clause], groupBy:List[String], order: Option[Direction]) {
  def this(select:Select, from:From, where: Option[Clause], order: Option[Direction]) = this(select, from, where, List(), order)
  def this(select:Select, from:String, joins:List[Join], where: Option[Clause], order: Option[Direction]) = this(select, From(RealTable(from), joins), where, List(), order)
  def innerJoin(table:String, clause:Clause): Query = new Query(select, From(from.table, from.joins ::: Join(RealTable(table), "inner", clause) :: Nil), where, groupBy, order)
  def innerJoin(query : Query, alias:String, clause:Clause) = new Query(select, From(from.table, from.joins ::: Join(QueryTable(query, alias), "inner", clause) :: Nil), where, groupBy, order)
  def leftJoin(table:String, clause:Clause): Query = new Query(select, From(from.table, from.joins ::: Join(RealTable(table), "left", clause) :: Nil), where, groupBy, order)
  def leftJoin(query:Query, alias:String, clause:Clause): Query = new Query(select, From(from.table, from.joins ::: Join(QueryTable(query, alias), "left", clause) :: Nil), where, groupBy, order)
  def where(clause:Clause): Query = new Query(select, from, Some(clause), groupBy, order)
  def where(clauses:List[Clause]): Query = new Query(select, from, Clause.join(clauses), groupBy, order)
  def groupBy(fields:List[String]) = new Query(select, from, where, fields, order)
  def groupBy(field:String) = new Query(select, from, where, List(field), order)
  def orderBy(dir: Direction): Query = new Query(select, from, where, groupBy, Some(dir))
  def fromAlias = from.table.alias
  def joins = from.joins
  def addJoins(joins:List[Join]) = new Query(select, from.addJoins(joins), where, groupBy, order)
  def and(clause: Clause) : Query = {
    assert(where.isDefined)
    new Query(select, from, Some(where.get.and(clause)), groupBy, order)
  }
  override def toString = {
    val q = new SqlRenderer().render(this)
  	"Query: " + q.query + "\nParameters: " + q.parameters 
  }
}

case class Modify(update:Update, set : Equals, where: Option[Clause]) {
  def where(clause:Clause) = new Modify(update, set, Some(clause))
}

case class From(table:Table, joins:List[Join]) {
  def addJoins(j:List[Join]) = new From(table, joins:::j)
}

case class Select(fields:String*) {
  def from(table: String) = new Query(this, From(RealTable(table), List()), None, None)
  def from(from:From) = new Query(this, from, None, None)
}

case class Join(table:Table, joinType:String, clause:Clause)

case class Update(table:Table) {
  def set(x : Equals) = new Modify(this, x, None)
}

abstract class Table {
  def alias:String
}
case class RealTable(name:String) extends Table {
  def alias = {
    val space = name.indexOf(' ')
    name.substring(space+1)
  }
}
case class QueryTable(query:Query, alias:String) extends Table

abstract class Clause {
  def andMaybe(otherClause: Option[Clause]): Clause = otherClause match {
    case None => this
    case Some(other) => And(this, other)
  }
  def and(otherField: Clause): Clause = And(this, otherField)
  def and(clauses: List[Clause]): Clause = (this /: clauses) { _ and _ }
  def or(otherField: Clause): Clause = Or(this, otherField)
}
object Clause {
  def join(clauses:List[Clause]) = {
    if (clauses.isEmpty) {
      None
    } else {
      Some((clauses.head /: clauses.tail) { _ and _ })
    }
  }
  def joinOr(clauses:List[Clause]) = {
    if (clauses.isEmpty) {
      None
    } else {
      Some((clauses.head /: clauses.tail) { _ or _ })
    }
  }
}

case class Equals(f: Field, v:Any) extends Clause
case class Like(f: Field, pattern:Any) extends Clause
case class NotLike(f: Field, pattern:Any) extends Clause
case class GreaterThan(f: Field, v:Any) extends Clause
case class GreaterThanOrEqual(f: Field, v:Any) extends Clause
case class LessThan(f: Field, v:Any) extends Clause
case class LessThanOrEqual(f: Field, v:Any) extends Clause
case class NotEquals(f: Field, v:Any) extends Clause
case class In(field: Field, v: Iterable[Any]) extends Clause
case class NotIn(field: Field, v:Iterable[Any]) extends Clause
case class And(val lClause:Clause, val rClause:Clause) extends Clause
case class Or(val lClause:Clause, val rClause:Clause) extends Clause
case class NotNull(f : Field) extends Clause
case class IsNull(f : Field) extends Clause
case class Not(val clause: Clause) extends Clause
object FalseClause extends Clause

abstract class Direction
case class Asc(field: String) extends Direction
case class Desc(field: String) extends Direction

case class Field(val name: String) {
  def eql(v: => Any) = Equals(this, v)
  def like(v: => Any) = Like(this, v)
  def notLike(v: => Any) = NotLike(this, v)
  def gte(v: => Any) = GreaterThanOrEqual(this, v)
  def gt(v: => Any) = GreaterThan(this, v)
  def lt(v: => Any) = LessThan(this, v)
  def lte(v: => Any) = LessThanOrEqual(this, v)
  def neq(v: => Any) = NotEquals(this, v)
  def in(v: => Iterable[Any]) = In(this, v)
  def notIn(v: => Iterable[Any]) = NotIn(this, v)
  def isNotNull() = NotNull(this)
  def isNull() = IsNull(this)
  def asc = Asc(this.name)
  def desc = Desc(this.name)

  override def toString = name
}

object QueryBuilder {
  implicit def string2field(f: String) = new Field(f)

  /** entrypoint for starting a select query */
  def select(fields:String*) = Select(fields:_*)

  def update(table:String) = Update(RealTable(table))
}

case class LiteralString(text:String)
case class AnObject(obj:Object)

object QueryBuilderMain {
  def main(args : Array[String]) {
    import QueryBuilder._

    val name = "asdf"
val q = (
          select("t.IDENTID,t.ctype,t.tradeid,t.CNAME,t.CPARTY,t.PORTFOLIO,t.TRADEDON,t.UPDATEDATE,t.ENTEREDBY,t.bs,t.AMOUNT,s1.AMOUNT1,t.UNITS,t.CURR2,t.CURR1,t.pStrike,t.MATURITY,t.UNITS,t.pStrike,t.MATURITY,s3.EXPIRY,t.DATE2,t.CP,s2.fixindex as fixindex,s2.StartDate,t.DATE3,s2.EndDate,t.wayup")
                  from ("t_trades t")
                  leftJoin ("T_STREAM s1", ("t.IDENTID" eql "s1.TRADEID") and ("s1.STREAMID" eql 1))
                  leftJoin ("T_STREAM s2", ("t.IDENTID" eql "s2.TRADEID") and ("s2.STREAMID" eql 2))
                  leftJoin ("T_STREAM s3", ("t.IDENTID" eql "s3.TRADEID") and ("s3.STREAMID" eql 3))
                  leftJoin ("T_REVAL_TRADES r", ("r.IDENTID" eql "t.IDENTID") and ("r.PROFILEID" eql 0))
                  where (("t.lastaction" notIn List("Abandoned", "ClosedOut"))
                          and ("r.reval_value" isNotNull) // hasn't been revaled so not part of VaR run
                          and ("t.ctype" neq 100)
                  and ("t.ctype" neq 101)

                  ) // we can't handle cash ledgers yet
          )
    val sql = new SqlRenderer().render(q)
    println(sql.query)
    println(sql.parameters)
  ;
  }
}
