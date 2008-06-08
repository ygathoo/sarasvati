package org.codemonk.wf.xml;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

@XmlEnum(String.class)
public enum XmlExternalArcType
{
  @XmlEnumValue("in")   IN,
  @XmlEnumValue ("out") OUT;
}
