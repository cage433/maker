package starling.http

import javax.servlet.http.HttpServletRequest
import scala.xml.{Text, Node, NodeSeq}
import starling.props.{PropsHelper, Props}

/**
 * Represents a web page. Holds the default template
 */

abstract class WebPage(httpRequest:HttpServletRequest) {

  def body():NodeSeq
  def messages():NodeSeq = {
    val text = session("message")
    if (text != null) {
      httpRequest.getSession.removeAttribute("message")
      Text(text)
    } else {
      NodeSeq.Empty
    }
  }

  /**
   * Ensures that relative links are correct given that the current page may or may not end with /
   * When passing in relativeUrl assume that the page url ends with /
   */
  def link(relativeUrl:String) = {
    if (httpRequest.getRequestURI.endsWith("/")) {
      relativeUrl
    } else {
      val slash = httpRequest.getRequestURI.lastIndexOf("/")
      val basename = httpRequest.getRequestURI.substring(slash+1)
      basename + "/" + relativeUrl
    }
  }
  
  def session(key:String):String = {
    session(key, "")
  }
  def session(key:String, defaultValue:String) = {
    val value = httpRequest.getSession.getAttribute(key)
    if (value == null) {
      defaultValue
    } else {
      value.asInstanceOf[String]
    }
  }
  def title:String
  def page():Node = {
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <meta http-equiv="content-type" content="text/html; charset=UTF-8" />

        <title>{PropsHelper.defaultProps.ServerName()}: {title}</title>
        <style type="text/css">
                .center {{
                  margin-left: auto;
                  margin-right: auto;
                }}
                body {{
                  background-color: {PropsHelper.defaultProps.ServerColour()}
                }}
                table {{
                  border-collapse: collapse;
                }}
                .reportcell {{
                  padding: 10px;
                  vertical-align: top;
                  text-align: center;
                  border-bottom: solid;
                  border-color: black;
                }}
                .reportcellleft {{
                  text-align: left;
                  padding: 10px;
                  vertical-align: top;
                  text-align: center;
                  border-bottom: solid;
                  border-color: black
                }}
                .left {{
                  text-align: left;
                }}
                .report-box {{
                  border-bottom: solid;
                  margin-bottom: 10px;
                }}
        </style>
      </head>
      <body>
          <h1>{PropsHelper.defaultProps.ServerName()}: {title}</h1>
          { body() }
      </body>
    </html>
  }
}