package org.codemonk.wf.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

@XmlAccessorType(XmlAccessType.FIELD)
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

  @Override
  public String toString ()
  {
    StringBuilder buf = new StringBuilder ();
    buf.append( "<arc to=\"" );
    buf.append( to );
    buf.append( "\"" );

    if ( name != null )
    {
      buf.append( " name=\"" );
      buf.append( name );
      buf.append( "\"" );
    }

    buf.append( "\"/>" );
    return buf.toString();
  }
}
