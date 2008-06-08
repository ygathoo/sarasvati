package org.codemonk.wf.xml;

import javax.xml.bind.annotation.XmlAttribute;

public class XmlArc
{
  @XmlAttribute (name="to", required=true)
  protected String to;

  @XmlAttribute (name="name", required=false)
  protected String name;

  public String getTo ()
  {
    return to;
  }

  public void setTo (String to)
  {
    this.to = to;
  }

  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }
}
