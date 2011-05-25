package starling.schemaevolution.system


import java.io.{BufferedInputStream, InputStream}
import java.sql.{DatabaseMetaData, ResultSet}
import starling.richdb.RichDB
import starling.db.DBWriter


object PatchUtils {

 def foreachTradeTable(f:(String)=>Unit) {
    List("TrinityTrade", "GalenaTrade", "EAITrade", "SoftmarTrade", "RefinedAssignment", "RefinedFixation", "IntradayTrades").foreach {
      table => f(table)
    }
  }

  def getFileFromClasspath(path: String): String = {
    val inputStream = getClass.getResourceAsStream(path)
    if (inputStream == null) {
      throw new Exception("Can't find resource " + path)
    }
    val bufferedInputStream = new BufferedInputStream(inputStream)

    //Function to read an entire input stream and return a string of it
    def readEntireInputStream(stringSoFar: String, inputStream:InputStream): String = {
      inputStream.read() match {
        case -1 => stringSoFar
        case character:Int => readEntireInputStream(stringSoFar + character.asInstanceOf[Char], inputStream)
      }
    }

    //Read the input stream
    readEntireInputStream("", bufferedInputStream)
  }

  //TODO:KMD This is a bit dodgy, the suggestion is that we se how this goes and if it causes problems we should run the
  //mysql script from the command line.  At the moment this script will fail if we have embedded ;'s in strings.  Or
  //if we start using triggers, stored procedures etc. Maybe we can come up with a better regular expression
  def executeUpdateSQLFileFromClasspath(writer: DBWriter, path: String) = {
    //Read in the sql file
    val sqlFile = getFileFromClasspath(path)
    executeUpdateSQL(writer, sqlFile)
  }

  def executeUpdateSQL(writer: DBWriter, sql: String) = {
    val sqlStatements = sql.split(";")
    //Filter out any ones which are entirely comprised of whitespace
    val filteredSQLStatements = sqlStatements.filter(currentStatement => !currentStatement.matches("""^\s*$"""))
    filteredSQLStatements.foreach(writer.update(_))
  }

  def checkTableExists(starling: RichDB, tableName: String): Boolean = {

    //Get the list of tables
    val databaseMetaData = starling.metadata
    val resultSet = databaseMetaData.getTables(null, null, "%", null)

    //Object to extract the table information
    def extractTableNames(tableNamesSoFar: List[String], resultSet: ResultSet): List[String] = {
      resultSet.next match {
        case true => {
          extractTableNames(resultSet.getString("TABLE_NAME").toUpperCase() :: tableNamesSoFar, resultSet)
        }
        case false => {

          //Close the result set
          resultSet.close

          //Return the list of table names
          tableNamesSoFar
        }
      }
    }

    //Get the list of tables in the schema
    val tableNames = extractTableNames(List(), resultSet)    

    //Check if the table is in the list
    tableNames.exists(currentTableName => currentTableName == tableName.toUpperCase)
  }

  def checkColumnAllowsNulls(starling: RichDB, tableName: String, columnName: String): Boolean = {

    //Get the list of tables
    val databaseMetaData = starling.metadata
    val resultSet = databaseMetaData.getColumns(null, null, tableName, columnName)

    //Take the first object from the results
    resultSet.first

    //Find if the column supports null
    val columnAllowsNulls = resultSet.getInt("NULLABLE") match {
      case DatabaseMetaData.columnNoNulls => false
      case DatabaseMetaData.columnNullable => true
      case DatabaseMetaData.columnNullableUnknown => throw new RuntimeException("Cannot detect if column [" + columnName + "] in table [" + tableName + "] alows nulls")
      case _ => throw new IllegalStateException("You should not be able to reach this bit of code")
    }

    //Close the resultset
    resultSet.close

    columnAllowsNulls
  }

  def assertTableExists(starling: RichDB, tableName: String) = {

    //Check if the table exists if it doesntt throw an exception
    if(!checkTableExists(starling, tableName)) {
      throw new RuntimeException("Table [" + tableName + "] does not exist")
    }
  }

  def assertColumnAllowsNulls(starling: RichDB, tableName: String, columnName: String) = {

    //Check if the column allows nulls if it doesn't throw an exception
    if(!checkColumnAllowsNulls(starling, tableName, columnName)) {
      throw new RuntimeException("Column [" + columnName + "] in table [" + tableName + "] does not allow nulls")
    }
  }

  def assertColumnDoesNotAllowNulls(starling: RichDB, tableName: String, columnName: String) = {

    //Check if the column allows nulls if it doesn't throw an exception
    if(checkColumnAllowsNulls(starling, tableName, columnName)) {
      throw new RuntimeException("Column [" + columnName + "] in table [" + tableName  + "] does allow nulls")
    }
  }
}