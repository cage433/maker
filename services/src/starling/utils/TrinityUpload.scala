package starling.utils

import java.net.{BindException, ServerSocket}
import com.jcraft.jsch.{Channel, ChannelExec, KeyPair, JSch}
import java.io.ByteArrayInputStream
import org.apache.commons.io.IOUtils
import util.Random
import starling.utils.ImplicitConversions._
import util.control.Breaks

/**
 * requires ssh on a windows machine with trinity installed
 *
 *
 * Install cygwin (run cygwin.com/setup.exe)
 *  The mirror http://cygwin.basemirror.de has already been though the trafigura cache
 *  then see http://www.petri.co.il/setup-ssh-server-vista.htm
 * On the machine create a c:\Starling\tmp directory
 * Trinity should be at c:\Trinity
 */

object TrinityUpload extends Breaks {

  def upload(fclLines:List[String]):String = {
    val fcl = fclLines.mkString("\r\n")
    val command = """
    TMP=/cygdrive/c/starling/tmp/%s;
    mkdir $TMP;
    mkdir $TMP/in;
    mkdir $TMP/processed;
    mkdir $TMP/logs;
    cat > $TMP/in/upload.fcl;
    cd $TMP;
    echo -n "C:\TRINITY\\MARKETDATABATCH.EXE SET PROFILE PUBLIC \"Full Curve[Current]\"" > $TMP/upload.cmd;
    echo -en "\r\n" >> $TMP/upload.cmd;
    echo -n "C:\TRINITY\LCHUPLOAD.exe" \""/sp:c:\starling\tmp\%s\in"\" \""/dp:c:\starling\tmp\%s\processed"\" /ua:False /uo:False /us:False /uf:True /ue:False \"/rp:public Full Curve\" >> $TMP/upload.cmd;
    echo -en "\r\n" >> $TMP/upload.cmd;
    /cygdrive/c/Trinity/killtrinity y y;
    /cygdrive/c/Trinity/batchrun TRUAT trynne trinity upload.cmd starling-trinity.log logs;
    echo --Rec-file-----------------
    cat logs/Rec*;
    echo --Err-file-----------------
    cat logs/Err*;
    """.replaceAll("%s", "trin-dir-" + Random.nextInt)
    execTrinity(command, fcl)
  }
  def execTrinity(command:String, input:String) = {
    exec("ttraflon2k182", "thomas.rynne", "Trinity2010", command, input)
  }
  def exec(host:String, username:String, password:String, command:String, input:String) = {
    val jsch = new JSch();
    //jsch.addIdentity("/home/stacy/.ssh/id_dsa")
    val session = jsch.getSession(username, host, 22)
    session.setConfig("StrictHostKeyChecking", "no")
    session.setPassword(password)
    session.connect()

    val channel = session.openChannel("exec")
    channel.asInstanceOf[ChannelExec].setCommand(command)
    channel.connect

    val in = channel.getInputStream
    val out = channel.getOutputStream
    out.write(input.getBytes)
    out.flush
    out.close

    val stdOut = new StringBuffer()
    val buffer = new Array[Byte](1024)
    breakable {
      while(true) {
        while (in.available > 0) {
          val i = in.read(buffer, 0, 1024)
          if (i < 0) break
          stdOut.append(new String(buffer, 0, i))
        }
        if(channel.isClosed) {
          break
        }
        Thread.sleep(500)
      }
    }

    channel.disconnect
    session.disconnect()
    stdOut.toString
  }

  def test() = {
    val fcl = "3300CAHD     110406FF000007794.140000000000000000000000000000000CN"
    val result = upload(List(fcl))
    println(result)
  }
  def main(args: Array[String]) {
    test()
  }

}






