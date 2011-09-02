package starling.utils.sql


import starling.dbx.DataSourceFactory

case class ConnectionParams(url: String, username: String, password: String) {

  def dataSource = DataSourceFactory.getDataSource(url, username, password)
}
