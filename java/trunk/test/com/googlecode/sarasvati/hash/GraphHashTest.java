/**
 * Created on Jul 20, 2009
 */
package com.googlecode.sarasvati.hash;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FilenameFilter;
import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;

public class GraphHashTest
{
  public static Map<String, String> hashesMap = new HashMap<String, String>();

  public static void loadWorkflows () throws Exception
  {
    XmlLoader xmlLoader = new XmlLoader();

    File basePath = new File( "common/test-wf/" );

    FilenameFilter filter = new FilenameFilter()
    {
      @Override
      public boolean accept (File dir, String name)
      {
        return name.endsWith( ".wf.xml" ) && !name.equals( "demo-example.wf.xml" );
      }
    };

    for ( File file : basePath.listFiles( filter ) )
    {
      XmlProcessDefinition pd = xmlLoader.translate( file );
      String digest = pd.getMessageDigest();
      hashesMap.put( pd.getName(), digest );
      System.out.println( pd.getName() + ": " + digest );
    }
  }

  public static void scanForChanges () throws Exception
  {
    XmlLoader xmlLoader = new XmlLoader();

    File basePath = new File( "common/test-wf/" );

    FilenameFilter filter = new FilenameFilter()
    {
      @Override
      public boolean accept (File dir, String name)
      {
        return name.endsWith( ".wf.xml" ) && !name.equals( "demo-example.wf.xml" );
      }
    };

    for ( File file : basePath.listFiles( filter ) )
    {
      XmlProcessDefinition pd = xmlLoader.translate( file );

      if ( !hashesMap.containsKey( pd.getName() ) )
      {
        System.out.println( pd.getName() + " is new" );
        hashesMap.put( pd.getName(), pd.getMessageDigest() );
      }
      else
      {
        String old = hashesMap.get( pd.getName() );
        String current = pd.getMessageDigest();
        if ( !old.equals( current ) )
        {
          System.out.println( pd.getName() + " has changed.\n\tOld: " + old + "\n\tCurrent: " + current );
          hashesMap.put( pd.getName(), current );
        }
      }
    }
  }

  public static void main (String[] args) throws Exception
  {
    loadWorkflows();
    while ( true )
    {
      System.out.println( "Press <enter> to scan for changes." );
      new BufferedInputStream( System.in ).read();
      scanForChanges();
      System.out.println( "Finished scanning" );
    }
  }
}
