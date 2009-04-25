package com.googlecode.sarasvati.visual.test;

import java.awt.Component;
import java.awt.image.BufferedImage;
import java.io.File;

import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.hibernate.Session;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.adapter.Function;
import com.googlecode.sarasvati.adapter.NodeAdapterManager;
import com.googlecode.sarasvati.example.hib.HibTestSetup;
import com.googlecode.sarasvati.hib.HibEngine;
import com.googlecode.sarasvati.visual.DefaultGraphLookAndFeel;
import com.googlecode.sarasvati.visual.graph.SarasvatiGraphScene;
import com.googlecode.sarasvati.visual.icon.DefaultNodeIcon;
import com.googlecode.sarasvati.visual.icon.TaskIcon;

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
            Icon icon = null;
            if ( "task".equals( node.getType() ) )
            {
              icon = new TaskIcon( node, null );
            }
            else
            {
              icon = new DefaultNodeIcon( node, null );
            }

            JLabel label = new JLabel( icon );
            label.setSize( icon.getIconWidth(), icon.getIconHeight() );
            return label;
          }
        });

    HibTestSetup.init();
    Session session = HibTestSetup.openSession();
    HibEngine engine = new HibEngine( session );

    Graph graph = engine.getRepository().getLatestGraph( "embedded-task-rej" );

    JPanel panel = new JPanel();
    panel.addNotify();

    SarasvatiGraphScene graphScene = new SarasvatiGraphScene( graph, new DefaultGraphLookAndFeel( false, true ) );

    Function<String, Widget> mapper = new Function<String, Widget>()
    {
      @Override
      public String apply(Widget param)
      {
        return null;
      }
    };

    panel.validate();

    graphScene.setupForExportOnHeadless();
    BufferedImage image = graphScene.export( new StringBuilder(), mapper, mapper );
    ImageIO.write( image, "gif", new File( "/home/paul/tmp/image.gif" ) );
    image.flush();
  }
}
