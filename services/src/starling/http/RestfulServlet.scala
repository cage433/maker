package starling.http

import starling.daterange.Day
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import starling.utils.CollectionUtils._
import starling.utils.Log
import scala.collection.immutable.TreeMap

/**
 * A service can be an instance of any class if the output is just text. For example you could pass in an instance
 * of String and this servlet would expose its methods.
 *
 * If you want to be able to return html or csv ouput the methods will need to accept a ResponseWriter as the first
 * argument.
 */

class RestfulServlet(service: AnyRef) extends HttpServlet {
  val methods = Map() ++ (for (method <- service.getClass.getDeclaredMethods) yield {
    method.getName + "/" + method.getParameterTypes.length -> method
  })

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {
    var paramList = (for (key <- request.getParameterNames) yield {
      (key.asInstanceOf[String], request.getParameterMap.get(key).asInstanceOf[Array[String]])
    }).toList.sortWith(_._1 < _._1)

    val paramMap = new TreeMap[String, Array[String]]() ++ paramList

    response.setContentType("text/plain")
    paramMap.get("method") match {
      case None => {
        val listOfMethods = List[String]() ++ methods.keySet
        response.getWriter().print(listOfMethods)
      }
      case Some(m) => {
        val methodName = m(0)

        // the output type
        val fileType = (paramMap.get("type") match {
          case Some(t) => t(0)
          case None => "txt"
        }).toString

        // we've already gotten method and type so remove them from params
        val params = (for ((key, value) <- paramList if !Set("method", "type").contains(key)) yield {
          val parsed = parse(key, value(0))
          parsed.asInstanceOf[Object]
        }).toList

        // if we want something other than txt output we'll need to pass in a write as the first arg
        val arity = fileType match {
          case "txt" => params.size
          case _ => 1 + params.size
        }

        val methodWithArity = methodName + "/" + arity

        methods.get(methodWithArity) match {
          case Some(m) => {
            findWriter(fileType, response) match {
              case None => {
                response.setStatus(HttpServletResponse.SC_OK)
                val str = m.invoke(service, params.toArray[Object] : _*)
                response.getWriter.print(str)
              }
              case Some(w) => {
                if(params.contains(null)) {
                  Log.info("Some null params for " + m + " , most likely someone editing an excel query: " + params)
                  w.start("Method: " + m + ", params: " + params)
                  w.finish
                } else {
                  m.invoke(service, (w :: params).toArray[Object] : _*)
                }
              }
            }
          }
          case None => {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND)
            response.getWriter().println("Method not found: " + methodWithArity)
          }
        }
      }
    }
  }

  private def findWriter(fileType : String, response: HttpServletResponse) : Option[ResponseWriter] = fileType match {
    case "txt" => None
    case "html" => Some(new HtmlWriter(response))
  }

  object Int {
    def unapply(s: String): Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
  }

  object Double {
    def unapply(s: String): Option[Double] = try {
      Some(s.toDouble)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
  }

  def parse(key: String, value: String) = {
    val ExcelParam = """(\[\".*\"\])""".r
    val DayParam = """([a-z0-9]+day)""".r
    value match {
      case ExcelParam(name) => null
      case _ => {
        key.toLowerCase match {
          case DayParam(paramName) => Day.fromExcel(value.toDouble)
          // try to convert the value from a string to its correct type
          case _ => value match {
            case Int(x) => x
            case Double(x) => x
            case Day(x) => x
            case _ => {
              value
            }
          }
        }
      }
    }
  }
}