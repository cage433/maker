package starling.http


import java.io.PrintWriter
import javax.servlet.http.HttpServletResponse

trait ResponseWriter {
  val status = HttpServletResponse.SC_OK

  def start(name: String)

  def startTable(headers: List[String])

  def writeRow(elements: List[Any])

  def endTable

  def finish
}

class HtmlWriter(response: HttpServletResponse) extends ResponseWriter {
  val out = response.getWriter()

  override def start(name: String) {
    response.setStatus(status)
    response.setContentType("text/html")
    out.print("<html><head><title>")
    out.print(name)
    out.println("</title>")
    out.println(" <style>")
    out.println("  table { border-collapse: collapse; border: 1px solid }")
    out.println("  td { border-style: solid}")
    out.println("  th { border-style: solid}")
    out.println("</style>")
    out.println("</head>")
    out.println("<body>")
    out.println("<!-- If you are trying to parse this you're doing something wrong. Talk to the Starling team -->")
  }

  override def startTable(headers: List[String]) {
    out.println("<table id=\"main\">")
    writeRow("th", headers)
  }

  override def writeRow(elements: List[Any]) {
    writeRow("td", elements)
  }

  private def writeRow(tag: String, elements: List[Any]) {
    out.print("<tr>")
    for (element <- elements) {
      out.print("<" + tag + ">" +
              element.toString +
              "</" + tag + ">")
    }
    out.println("</tr>")
  }

  override def endTable {
    out.println("</table>")
  }

  override def finish = {
    out.println("</table>")
    out.println("</body>")
    out.println("</html>")
  }
}
