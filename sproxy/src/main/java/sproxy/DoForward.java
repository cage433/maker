package sproxy;

// ========================================================================
// $Id: ProxyServlet.java,v 1.2 2004/07/19 13:13:00 hlavac Exp $
// Copyright 2004-2004 Mort Bay Consulting Pty. Ltd.
// ------------------------------------------------------------------------
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ========================================================================

//Hacked so that it is a static method instead of a servlet
//Also added rewriting of the redirect location header

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Enumeration;
import java.util.HashSet;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.mortbay.util.IO;

public class DoForward {
	
	public DoForward() {}
	
    protected HashSet  _DontProxyHeaders = new HashSet ();
    {
        _DontProxyHeaders.add("proxy-connection");
        _DontProxyHeaders.add("connection");
        _DontProxyHeaders.add("keep-alive");
        _DontProxyHeaders.add("transfer-encoding");
        _DontProxyHeaders.add("te");
        _DontProxyHeaders.add("trailer");
        _DontProxyHeaders.add("proxy-authorization");
        _DontProxyHeaders.add("proxy-authenticate");
        _DontProxyHeaders.add("upgrade");
    }
    
    public void forward(
    		ServletContext context,
    		ServletRequest req,
    		ServletResponse res,
    		Integer port,
    		String prefix)
    	throws ServletException, IOException 
    {
        HttpServletRequest  request = (HttpServletRequest )req;
        HttpServletResponse  response = (HttpServletResponse )res;
        if ("CONNECT".equalsIgnoreCase(request.getMethod()))
        {
       	 throw new RuntimeException("Don't know what connect is");
//            handleConnect(request,response);
        }
        else
        {
       	    String serverName = request.getServerName();
            String path = request.getRequestURI();
            String queryString = request.getQueryString();

            context.log("Path="+path);
            
            serverName = "localhost";
            path = path.substring(prefix.length()+1);
            
            context.log("Path after="+path);
            //build url...
            String  uri=path;
            if (queryString!=null) {
                uri+="?"+queryString;
            }
            String scheme = request.getScheme();
            URL  url = new URL(scheme, serverName, port.intValue(), uri);//"zz");//null;//new java.net.URL(request.getScheme(), serverName, port, uri);
            
            context.log("URL="+url);

            
            
            URLConnection  connection = url.openConnection();
            connection.setAllowUserInteraction(false);
            
            // Set method
            HttpURLConnection  http = null;
            if (connection instanceof HttpURLConnection )
            {
                http = (HttpURLConnection )connection;
                http.setRequestMethod(request.getMethod());
                http.setInstanceFollowRedirects(false);
            }

            // check connection header
            String  connectionHdr = request.getHeader("Connection");
            if (connectionHdr!=null)
            {
                connectionHdr=connectionHdr.toLowerCase();
                if (connectionHdr.equals("keep-alive")||
                    connectionHdr.equals("close"))
                    connectionHdr=null;
            }
            
            // copy headers
            boolean xForwardedFor=false;
            boolean hasContent=false;
            Enumeration  enm = request.getHeaderNames();
            while (enm.hasMoreElements())
            {
                // TODO [19 Nov 2009] could be better than this!
            	String  hdr=(String )enm.nextElement();
                String  lhdr=hdr.toLowerCase();

                if (_DontProxyHeaders.contains(lhdr))
                    continue;
                if (connectionHdr!=null && connectionHdr.indexOf(lhdr)>=0)
                    continue;

                if ("content-type".equals(lhdr))
                    hasContent=true;

                Enumeration  vals = request.getHeaders(hdr);
                while (vals.hasMoreElements())
                {
                    String  val = (String )vals.nextElement();
                    if (val!=null)
                    {
                        connection.addRequestProperty(hdr,val);
                        context.log("req "+hdr+": "+val);
                        xForwardedFor|="X-Forwarded-For".equalsIgnoreCase(hdr);
                    }
                }
            }

            // Proxy headers
            connection.setRequestProperty("Via","1.1 (jetty)");
            if (!xForwardedFor)
                connection.addRequestProperty("X-Forwarded-For",
                                              request.getRemoteAddr());

            // a little bit of cache control
            String  cache_control = request.getHeader("Cache-Control");
            if (cache_control!=null &&
                (cache_control.indexOf("no-cache")>=0 ||
                 cache_control.indexOf("no-store")>=0))
                connection.setUseCaches(false);

            // customize Connection

            try
            {
                connection.setDoInput(true);
                
                // do input thang!
                InputStream  in=request.getInputStream();
                if (hasContent)
                {
                    connection.setDoOutput(true);
                    IO.copy(in,connection.getOutputStream());
                }
                try {
                	// Connect
                	connection.connect();
                } catch (ConnectException e) {
                	e.printStackTrace();
                	response.setStatus(500);
                	printConnectExceptionPage(response, port, prefix);
                	return;
                }
            }
            catch (Exception  e)
            {
                context.log("proxy",e);
            }
            
            InputStream  proxy_in = null;

            // handler status codes etc.
            int code=500;
            if (http!=null)
            {
                proxy_in = http.getErrorStream();
                
                code=http.getResponseCode();
                response.setStatus(code,http.getResponseMessage());
                context.log("response = "+http.getResponseCode());
            }
            
            if (proxy_in==null)
            {
                try {proxy_in=connection.getInputStream();}
                catch (Exception  e)
                {
                    context.log("stream",e);
                    proxy_in = http.getErrorStream();
                }
            }
            
            // clear response defaults.
            response.setHeader("Date",null);
            response.setHeader("Server",null);
            
            // set response headers
            int h=0;
            String  hdr=connection.getHeaderFieldKey(h);
            String  val=connection.getHeaderField(h);
            while(hdr!=null || val!=null)
            {
                String  lhdr = hdr!=null?hdr.toLowerCase():null;
                if (hdr!=null && val!=null && !_DontProxyHeaders.contains(lhdr)) {
                    if ("location".equals(lhdr)) {
                        val = modify(val,port,prefix);
                    }
                	response.addHeader(hdr,val);
                }

                context.log("res "+hdr+": "+val);
                
                h++;
                hdr=connection.getHeaderFieldKey(h);
                val=connection.getHeaderField(h);
            }
            response.addHeader("Via","1.1 (jetty)");

            // Handle
            if (proxy_in!=null)
                IO.copy(proxy_in,response.getOutputStream());
            
        }
    }

	private String modify(String location, Integer port, String prefix) {
		try {
			URL url = new URL(location);
			URL rewrite = new URL(
					url.getProtocol(),
					url.getHost(),
					url.getPort(),
					"/" + prefix + url.getPath());
			System.out.println(url + " " + rewrite);
			return rewrite.toString();
		} catch (MalformedURLException e) {
			throw new RuntimeException(e);
		}
	}

	private void printConnectExceptionPage(HttpServletResponse response,
			Integer port, String prefix) throws IOException {
		PrintWriter writer = response.getWriter();
		writer.println("<html> <head>");
		writer.println(" <title> Can't connect to " + prefix + "</title>");
		writer.println("</head>");
		writer.println("<body>");
		writer.println(" <h1>"  + prefix + "</h1>");
		writer.println(" <p>Can't connect to " + prefix + ".<br/>");
		writer.println("    The server is probably not running.</p>");
		writer.println("<br/>");
		writer.println("<br/>");
		writer.println("<p>Details: port - " + port + "</p>");
		writer.println("</body>");
		writer.println("</html>");
	}
}
