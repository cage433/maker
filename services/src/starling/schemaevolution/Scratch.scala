package starling.schemaevolution

import xml.{Utility, Text, Elem}

object Scratch {

  def main(args:Array[String]) {
    val sample = <map>
      <ordering class="scala.math.Ordering$String$"></ordering>
      <entry> <string>A</string><value>1</value> </entry>
      <entry> <string>B</string><value>1</value> </entry>
      <entry> <string>C</string><value>1</value> </entry>
    </map>


    val f = sample.asInstanceOf[Elem].child.flatMap(Utility.trimProper).filter {
      case <ordering></ordering> => true
      case <entry><string>{Text(t)}</string>{_}</entry> => { t == "B" }
      case _ => false
    }

    val x = sample.copy(child=f)
    println(x)

    //f.foreach(x => println("> " + x))
  }
}