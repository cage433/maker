package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.gui.api.{EAIDeskInfo, Desk}


class Patch148_AddNewBookForCED extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    NewBook.create(starling, Desk.CED.deskInfo.get.asInstanceOf[EAIDeskInfo].book)
  }

  object NewBook {
    def create(starling: RichDB, id: Int) {
      starling.inTransaction {
        writer => {
          val query = """
            select *
            into EAITrade_book_""" + id + """
            from EAITrade_book_22
            where id = -1
          """
          writer.update(query)
        }
      }
    }
  }
}
