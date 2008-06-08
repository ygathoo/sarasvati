package org.codemonk.wf.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
public class XmlWorkflow
{
  @XmlAttribute (name="name", required=true)
  protected String name;

  @XmlElement (name="node")
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
}
