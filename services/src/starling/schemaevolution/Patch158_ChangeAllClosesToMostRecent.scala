package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.dbx.QueryBuilder._
import starling.utils.RichString
import starling.props.ServerTypeLabel

class Patch158_ChangeAllClosesToMostRecent extends Patch with RichString{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {

    if (starlingInit.props.ServerType() == ServerTypeLabel.FC2) {
      val bookmarksToUpdate = starling.queryWithResult("""select * from Bookmarks where bookmark like '%<environmentRule><name>All Closes</name></environmentRule>%'""") {
        res =>
          val name = res.getString("bookmarkName")
          val user = res.getString("starlingUser")
          val bundle = res.getString("bundle")
          val bookmark = res.getString("bookmark")
          (name, user, bundle, bookmark)
      }

      def escapeQuotes(str: String) = str.replace("'", "''")
      bookmarksToUpdate.foreach {
        case (name, user, bundle, bookmark) =>
          val sql = """update Bookmarks set Bookmark = '%s' where bookmarkName = '%s' and starlingUser = '%s' and bundle = '%s'""" %(
            bookmark.replace("""<environmentRule><name>All Closes</name></environmentRule>""", """<environmentRule><name>Most Recent Closes</name></environmentRule>"""),
            escapeQuotes(name),
            user,
            bundle
            )
          writer.update(sql)

      }
    }
  }
}
