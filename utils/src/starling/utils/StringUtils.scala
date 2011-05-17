package starling.utils


object StringUtils {
  def escapeForCSV(any: Any): String = {
    var res = any.toString
    res = res.replace("\"", "\"\"")
    if(res.contains(",") || res.contains("\n") || res.contains("\""))
      res = "\"" + res + "\""
    res
  }
}