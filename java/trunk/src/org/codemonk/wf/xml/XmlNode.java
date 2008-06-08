package org.codemonk.wf.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

public class XmlNode
{
  @XmlAttribute (name="name", required=true)
  protected String name;

  @XmlAttribute (name="isJoin", required=false)
  protected Boolean join;

  @XmlAttribute (name="type", required=false)
  protected String type;

  @XmlAttribute (name="isStart", required=false)
  protected Boolean start;

  @XmlElement (name="guard",required=false)
  protected String guard;

  @XmlElement (name="arc",required=false)
  protected List<XmlArc> arcs = new ArrayList<XmlArc>();

  @XmlElement (name="externalArc",required=false)
  protected List<XmlExternalArc> externalArcs = new ArrayList<XmlExternalArc>();

  public String getName()
  {
    return name;
  }

  public void setName( String name )
  {
    this.name = name;
  }

  public Boolean getJoin()
  {
    return join;
  }

  public void setJoin( Boolean join )
  {
    this.join = join;
  }

  public String getType()
  {
    return type;
  }

  public void setType( String type )
  {
    this.type = type;
  }

  public Boolean getStart()
  {
    return start;
  }

  public void setStart( Boolean start )
  {
    this.start = start;
  }

  public String getGuard()
  {
    return guard;
  }

  public void setGuard( String guard )
  {
    this.guard = guard;
  }

  public List<XmlArc> getArcs()
  {
    return arcs;
  }

  public void setArcs( List<XmlArc> arcs )
  {
    this.arcs = arcs;
  }

  public List<XmlExternalArc> getExternalArcs()
  {
    return externalArcs;
  }

  public void setExternalArcs( List<XmlExternalArc> externalArcs )
  {
    this.externalArcs = externalArcs;
  }
}
