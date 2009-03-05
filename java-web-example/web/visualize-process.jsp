<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<%@page import="com.googlecode.sarasvati.example.web.TestSetup"%>
<%@page import="org.hibernate.Session"%>
<%@page import="com.googlecode.sarasvati.*"%>
<%@page import="com.googlecode.sarasvati.hib.*"%>
<%@page import="com.googlecode.sarasvati.visual.*"%>
<%@page import="com.googlecode.sarasvati.visual.process.*"%>
<%@page import="java.text.SimpleDateFormat"%><html>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<%
  Session hibSession = TestSetup.openSession();

  String mapContents = null;
  GraphProcess process = null;

  String basePath = config.getServletContext().getRealPath( "/" );
  String name = "/" + System.currentTimeMillis() + ".gif";

  ProcessImageMapCreator imageMapCreator = null;

  try
  {
    HibEngine hibEngine = new HibEngine( hibSession );
    process = hibEngine.getRepository().findProcess( 3 );

    final SimpleDateFormat sdf = new SimpleDateFormat( "yyyy-MM-dd HH:mm:ss" );

    ProcessToImageMapAdapter helper = new ProcessToImageMapAdapter ()
    {
      public String hrefForNode (VisualProcessNode node)
      {
        return "javascript:alert( 'You have selected " + node.getNode().getName() + "' );";
      }

      public String hoverForNode (VisualProcessNode node)
      {
        NodeToken token = node.getToken();
        if ( token == null )
        {
          return null;
        }
        return "Started: " + sdf.format( token.getCreateDate() ) +
               " Finished: " + (token.getCompleteDate() == null ? "Not yet finished" : sdf.format( token.getCompleteDate() ) ) ;
      }
    };

    imageMapCreator = new ProcessImageMapCreator( process, helper );
    imageMapCreator.writeImageToFile( "gif", basePath + name );
  }
  finally
  {
    hibSession.close();
  }
%>

<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Visualize Process</title>
</head>
<body>

  <map name="processMap">
    <%=imageMapCreator.getMapContents()%>
  </map>

  <div style="margin-left:10px; padding-top:10px">
    <image style="border:2px black solid" src="<%=request.getContextPath() + name%>" usemap="#processMap"/>
  </div>

</body>
</html>