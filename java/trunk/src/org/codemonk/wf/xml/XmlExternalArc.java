package org.codemonk.wf.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

@XmlAccessorType(XmlAccessType.FIELD)
public class XmlExternalArc
{
  @XmlAttribute(name = "external", required = true)
  protected String             external;

  @XmlAttribute(name = "instance", required = true)
  protected String             instance;

  @XmlAttribute(name = "nodeName", required = true)
  protected String             nodeName;

  @XmlAttribute(name = "name", required = false)
  protected String             name;

  @XmlAttribute(name = "type", required = true)
  protected XmlExternalArcType type;

  public String getExternal ()
  {
    return external;
  }

  public void setExternal (String external)
  {
    this.external = external;
  }

  public String getInstance ()
  {
    return instance;
  }

  public void setInstance (String instance)
  {
    this.instance = instance;
  }

  public String getNodeName ()
  {
    return nodeName;
  }

  public void setNodeName (String nodeName)
  {
    this.nodeName = nodeName;
  }

  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public XmlExternalArcType getType ()
  {
    return type;
  }

  public void setType (XmlExternalArcType type)
  {
    this.type = type;
  }

  @Override
  public String toString ()
  {
    StringBuilder buf = new StringBuilder ();
    buf.append( "<externalArc external=\"" );
    buf.append( external );
    buf.append( "\" instance=\"" );
    buf.append( instance );
    buf.append( "\" nodeName=\"" );
    buf.append( nodeName );

    if ( name != null )
    {
      buf.append( "\" name=\"" );
      buf.append( name );
    }

    buf.append( "\" type=\"" );
    buf.append( type.name().toLowerCase() );

    buf.append( "\"/>" );
    return buf.toString();
  }
}
