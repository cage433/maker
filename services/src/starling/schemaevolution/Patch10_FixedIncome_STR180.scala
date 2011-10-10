package starling.schemaevolution

import system.Patch
import starling.instrument.utils.StarlingXStream
import xstream.{Fields, Reader, MapBasedConverter, ConvertMarketData}
import starling.quantity.Percentage
import starling.marketdata.{ImpliedVolEntryKey, ImpliedVolData}
import collection.SortedMap
import starling.daterange.Day
import starling.richdb.{RichDB}
import starling.db.DBWriter
import starling.props.Props
import java.sql.{Statement, ResultSet, Connection}
import starling.services.StarlingInit

class Patch10_FixedIncome_STR180 extends Patch{
//
//  val converter = new MapBasedConverter(
//    StarlingXStream.createXStream,
//    classOf[ImpliedVolData],
//    new Reader(){
//      def create(fields: Fields) = {
//        val surface = fields.getFieldValue("surface").asInstanceOf[Option[SortedMap[ImpliedVolEntryKey, Percentage]]].get
//        ImpliedVolData(surface)
//      }
//    },
//    Map.empty[String, Class[_]]
//  )
//  val converter2 = new MapBasedConverter(
//    StarlingXStream.createXStream,
//    classOf[ImpliedVolEntryKey],
//    new Reader(){
//      def create(fields: Fields) = {
//        val exerciseDay =fields.getFieldValue("exerciseDay").asInstanceOf[Option[Day]].get
//        val lastTrading =fields.getFieldValue("lastTrading").asInstanceOf[Option[Day]].get
//        val strike      =fields.getFieldValue("strike").asInstanceOf[Option[Double]].get
//        ImpliedVolEntryKey(lastTrading, strike, exerciseDay)
//      }
//    },
//    Map[String, Class[_]]("lastTrading" -> classOf[Day])
//  )
//
//  val standardXStream = StarlingXStream.createXStream
//
//  val convertingXStream = StarlingXStream.createXStream
//  convertingXStream.registerConverter(converter)
//  convertingXStream.registerConverter(converter2)


  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
//    var connection : Connection = null
//    var rs : ResultSet = null
//    var statement : Statement = null
//    try {
//      connection = Props.StarlingDatabase().dataSource.getConnection
//      try {
//        statement = connection.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_UPDATABLE)
//
//        try {
//          val queryString = "Select * from SnapshotData where dataTypeKey = 'ImpliedVolData'"
//          rs = statement.executeQuery(queryString)
//
//          while(rs.next){
//            val snapshotID = rs.getInt("snapshotID")
//            val subTypeKey = rs.getString("subTypeKey")
//            val xml = rs.getString("data")
//            val convertedObject = convertingXStream.fromXML(xml)
//            val newXML = standardXStream.toXML(convertedObject)
//
//            println("Converting " + snapshotID + ", " + subTypeKey)
//            rs.updateString("data", newXML)
//            rs.updateRow
//          }
//        } finally {
//          if (rs != null)
//            rs.close
//        }
//      } finally {
//        if (statement != null)
//          statement.close
//      }
//    } finally {
//      if (connection != null)
//        connection.close
//    }
  }

  def patchDescription() = "Remove uom field from Implied Vol Data"
}