package org.codemonk.wf.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement (name="workflow",namespace="http://sarasvati.googlecode.com/workflow/")
@XmlAccessorType(XmlAccessType.FIELD)
public class XmlWorkflow
{
  @XmlAttribute (name="name", required=true)
  protected String name;

  @XmlElement (name="node", namespace="http://sarasvati.googlecode.com/workflow/")
  protected List<XmlNode> nodes = new ArrayList<XmlNode>();

  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public List<XmlNode> getNodes ()
  {
    return nodes;
  }

  public void setNodes (List<XmlNode> nodes)
  {
    this.nodes = nodes;
  }

  @Override
  public String toString ()
  {
    StringBuilder buf = new StringBuilder ();
    buf.append( "<workflow name=\"" );
    buf.append( getName() );
    buf.append( "\">\n" );

    for ( XmlNode node : nodes )
    {
      buf.append( node );
      buf.append( "\n" );
    }

    buf.append( "</workflow>" );
    return buf.toString();
  }
}
