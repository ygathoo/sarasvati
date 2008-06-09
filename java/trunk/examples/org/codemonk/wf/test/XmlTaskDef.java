/**
 * Created on Jun 9, 2008
 */
package org.codemonk.wf.test;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name="task-def", namespace="http://sarasvati.googlecode.com/example/")
@XmlAccessorType(XmlAccessType.FIELD)
public class XmlTaskDef
{
  @XmlElement (name="task-name",namespace="http://sarasvati.googlecode.com/example/", required=true)
  protected String taskName;

  @XmlElement (name="description",namespace="http://sarasvati.googlecode.com/example/", required=true)
  protected String description;

  public String getTaskName ()
  {
    return taskName;
  }

  public void setTaskName (String taskName)
  {
    this.taskName = taskName;
  }

  public String getDescription ()
  {
    return description;
  }

  public void setDescription (String description)
  {
    this.description = description;
  }
}
