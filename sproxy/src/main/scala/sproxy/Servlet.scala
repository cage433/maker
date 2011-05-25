package sproxy

import javax.servlet.http._
import scala.collection.mutable.LinkedList
import scala.xml.Node

/*
 * This holds the main (and only) servlet which serves the
 * root and admin page as well as the proxing behaviour.
 * 
 * The actual proxying behaviour is in DoForward which is
 * a modified form of ProxyServlet from jetty.
 */

class MainServlet(deployments:Deployments, reload:Boolean) extends HttpServlet {
  override def doGet(request:HttpServletRequest, response:HttpServletResponse) {
    println("rl " + reload)
    if (reload) {
      deployments.reload()
    }
    val path = request.getRequestURI
    if (path.equals("/")) {
      root(request, response)
    } else {
      val slash = path.substring(1).indexOf("/")
      val firstPath =
        if (slash.equals(-1)) { path.substring(1) } else { path.substring(1,slash+1)}
      if (firstPath == "Admin") {
        admin(request, response)
      } else {
        forward(request, response)
      }
    }
  }
  //any GET or POST calls which should be proxies go here
  def forward(request:HttpServletRequest, response:HttpServletResponse) {
    val path = request.getRequestURI
    val slash = path.substring(1).indexOf("/")
    val firstPath =
        if (slash.equals(-1)) {
          path.substring(1)
        } else {
          path.substring(1,slash+1)
        }
    val portOrNone = deployments.lookup(firstPath)
    portOrNone match {
          case None =>
            response.setStatus(404)
            response.getWriter.println("404 page not found")
            response.getWriter.println()
            response.getWriter.println("Valid prefixes:")
            deployments.valid_deployments.foreach( (name) => {
                response.getWriter.println(" " + name)
            })
            
          case Some(port) => {
            if (path.equals("/" + firstPath)) {
              response.sendRedirect(firstPath + "/")
            } else {
             new DoForward().forward(
              getServletContext,
              request,
              response,
              port,
              firstPath)
             }
          }
    }
  }  
  def root(request:HttpServletRequest, response:HttpServletResponse) {
    val deploymentList = deployments.dirs.toArray(Array[Deployment]()).map( (deployment) => {
      deployment match {
        case ValidDeployment(name,dir,port,deploymentDetails) =>
          <li><a href={name}>{name}</a></li>
        case ErrorDeployment(name,dir,message,deploymentDetails) => <span/>
      }
    })
    var message:Node = null
    if (deployments.nothing) {
      message = <p>No deployments found. The directory is empty</p>  
    } else if (deployments.noValidDeployments) {
      message = <p>No valid deployments found. See <a href='Admin'>Admin</a></p>
    }
    val page =
    <html>
     <head><title>{deployments.name}</title></head>
     <body>
       <h1>{deployments.name} deployments</h1>
       <div style='float:right'><a href='Admin'>Admin</a></div>
       <ul>
       {deploymentList}
       </ul>
       {message}
     </body>
    </html>
    response.setContentType("text/html")
    response.getWriter.println(page)
  }
  def admin(request:HttpServletRequest, response:HttpServletResponse) {
    val deploymentList = deployments.dirs.toArray(Array[Deployment]()).map( (deployment) => {
      deployment match {
        case ValidDeployment(name,dir,port,deploymentDetails) =>
          <tr>
            <td><a href={name}>{name}</a></td>
            <td>{dir}</td>
            <td>{port}</td>
            <td>&nbsp;</td>
            <td>{deploymentDetails}</td>
          </tr>
        case ErrorDeployment(name,dir,message,depoymentDetails) =>
          <tr>
            <td>{name}</td>
            <td>{dir}</td>
            <td>&nbsp;</td>
            <td>{message}</td>
            <td>{depoymentDetails}</td>
          </tr>
      }
    })
    val page =
    <html>
     <head>
       <title>{deployments.name} Admin</title>
       <style type="text/css"><![CDATA[
         td { }
       ]]></style>
     </head>
     <body>
       <h1>{deployments.name} Admin</h1>
       <div style='float:right'>(<a href='/'>Home</a>)</div>
       <table style="border: solid; margin: 2em">
         <tr><th>Name</th><th>Dir</th><th>Port</th><th>Status</th><th>deployment_details</th></tr>
       {deploymentList}
       </table>
       <form action='Reload' method='Post'>
         <input type="Submit" value="Reload"/>
       </form>
     </body>
    </html>
    response.setContentType("text/html")
    response.getWriter.println(page)
  }  
  override def doPost(request:HttpServletRequest, response:HttpServletResponse) {
    val path = request.getRequestURI
    if (path.equals("/Reload")) {
      deployments.reload
      response.sendRedirect("Admin")
    } else {
      forward(request,response)
    }
  }  
}

