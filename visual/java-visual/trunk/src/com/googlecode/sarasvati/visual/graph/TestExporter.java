package com.googlecode.sarasvati.visual.graph;

import java.awt.Component;

import javax.swing.JLabel;

import org.hibernate.Session;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.adapter.Function;
import com.googlecode.sarasvati.adapter.NodeAdapterManager;
import com.googlecode.sarasvati.example.db.TestSetup;
import com.googlecode.sarasvati.hib.HibEngine;
import com.googlecode.sarasvati.visual.DefaultNodeIcon;
import com.googlecode.sarasvati.visual.TaskIcon;

public class TestExporter
{
  public static void main(String[] args) throws Exception
  {
    NodeAdapterManager.registerFactory( Component.class,
        new Function<Component, Node>()
        {
          @Override
          public Component apply (Node node)
          {
            if ( "task".equals( node.getType() ) )
            {
              return new JLabel( new TaskIcon( node, null ) );
            }

            return new JLabel( new DefaultNodeIcon( node, null ) );
          }
        });

    TestSetup.init();
    Session session = TestSetup.openSession();
    HibEngine engine = new HibEngine( session );

    Graph graph = engine.getRepository().getLatestGraph( "embedded-task-rej" );

    SarasvatiGraphScene graphScene = new SarasvatiGraphScene( graph );

    Function<String, Widget> mapper = new Function<String, Widget>()
    {
      @Override
      public String apply(Widget param)
      {
        return null;
      }
    };

    graphScene.export( new StringBuilder(), mapper, mapper );
  }
}
