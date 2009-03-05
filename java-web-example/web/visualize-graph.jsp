<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<%@page import="com.googlecode.sarasvati.example.web.TestSetup"%>
<%@page import="org.hibernate.Session"%>
<%@page import="com.googlecode.sarasvati.*"%>
<%@page import="com.googlecode.sarasvati.hib.*"%>
<%@page import="com.googlecode.sarasvati.visual.*"%>

<html>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<%
  Session hibSession = TestSetup.openSession();

  String mapContents = null;
  Graph graph = null;

  String basePath = config.getServletContext().getRealPath( "/" );

  String name = "/" + System.currentTimeMillis() + ".gif";

  GraphImageMapCreator imageMapCreator = null;

  try
  {
    HibEngine hibEngine = new HibEngine( hibSession );
    graph = hibEngine.getRepository().getLatestGraph( "embedded-task-rej" );

    GraphToImageMapAdapter helper = new GraphToImageMapAdapter ()
    {
      @Override public String hrefForNode (Node node)
      {
        return "javascript:alert( 'You have selected " + node.getName() + "' );";
      }

      @Override public String hoverForNode (Node node)
      {
        return "Name: " + node.getName() + ",  Type: " + node.getType() +
               ",  Guard: " + node.getGuard() + ",  Is start: " + node.isStart() +
               ",  Is join: " + node.isJoin();
      }
    };

    imageMapCreator = new GraphImageMapCreator( graph, helper );
    imageMapCreator.writeMapImageToFile( "gif", basePath + name );
  }
  finally
  {
    hibSession.close();
  }
%>

<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Visualize Graph</title>
</head>
<body>

  <map name="graphMap">
    <%=imageMapCreator.getMapContents()%>
  </map>

  <div style="margin-left:10px; padding-top:10px">
    <image style="border:2px black solid" src="<%=request.getContextPath() + "/" + name%>" usemap="#graphMap"/>
  </div>

</body>
</html>