package com.googlecode.sarasvati.visual.test;

import java.io.File;
import java.io.FilenameFilter;

import org.hibernate.Session;

import com.googlecode.sarasvati.CustomNode;
import com.googlecode.sarasvati.example.hib.HibTestSetup;
import com.googlecode.sarasvati.hib.HibEngine;
import com.googlecode.sarasvati.hib.HibGraph;
import com.googlecode.sarasvati.hib.HibNode;
import com.googlecode.sarasvati.impl.WaitNode;
import com.googlecode.sarasvati.load.GraphLoader;
import com.googlecode.sarasvati.visual.AbstractProcessVisualizer;
import com.googlecode.sarasvati.xml.DefaultFileXmlProcessDefinitionResolver;
import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlProcessDefinitionResolver;

public class TestRejectNodeProcessVisualizer extends AbstractProcessVisualizer
{
  @Override
  public void init () throws Exception
  {
    HibTestSetup.init( false );
  }

  @Override
  public Session getSession ()
  {
    return HibTestSetup.openSession();
  }

  public static void main (String[] args) throws Exception
  {
    load();
    completeProcess();
    new TestRejectNodeProcessVisualizer().run();
  }

  private static void completeProcess ()
  {
    Session session = HibTestSetup.openSession();
    session.beginTransaction();
    HibEngine engine = new HibEngine( session );

    /*
     * Select Exam Approval PD graph and start process
     */
    HibGraph graph = engine.getRepository().getLatestGraph( "reject-node" );
    engine.startProcess( graph );
    session.getTransaction().commit();
  }

  private static void load () throws Exception
  {
    HibTestSetup.init( false );

    Session sess = HibTestSetup.openSession();
    sess.beginTransaction();

    HibEngine engine = new HibEngine( sess );
    XmlLoader xmlLoader = new XmlLoader();

    engine.addNodeType( "node", HibNode.class );
    engine.addNodeType( "custom", CustomNode.class );
    engine.addNodeType( "wait", WaitNode.class );

    GraphLoader<HibGraph> wfLoader = engine.getLoader();

    File baseDir = new File( "test/com/googlecode/sarasvati/visual/test/" );
    assert baseDir.exists() : "Workflow process def dir not found.";

    XmlProcessDefinitionResolver resolver = new DefaultFileXmlProcessDefinitionResolver( xmlLoader,
        baseDir );

    FilenameFilter filter = new FilenameFilter()
    {
      @Override
      public boolean accept (File dir, String name)
      {
        return name.endsWith( ".wf.xml" ) && name.equals( "reject-node.wf.xml" );
      }
    };

    for ( File file : baseDir.listFiles( filter ) )
    {
      String name = file.getName();
      name = name.substring( 0, name.length() - ".wf.xml".length() );

      try
      {
        wfLoader.loadWithDependencies( name, resolver );
        System.out.println( "Loaded " + name );
      }
      catch ( Exception t )
      {
        System.out.println( "Failed to load: " + name + "  because: " + t.getMessage() );
        t.printStackTrace();
        return;
      }
    }

    sess.getTransaction().commit();
  }
}