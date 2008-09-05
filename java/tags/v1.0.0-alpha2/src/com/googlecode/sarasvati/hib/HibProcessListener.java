package com.googlecode.sarasvati.hib;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.googlecode.sarasvati.Process;
import com.googlecode.sarasvati.event.ExecutionEventType;

@Entity
@Table (name="wf_process_listener")
public class HibProcessListener
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long id;

  protected String type;

  @Column (name="event_type")
  protected ExecutionEventType eventType;

  @ManyToOne(fetch = FetchType.LAZY, targetEntity=HibProcess.class)
  @JoinColumn(name = "process_id")
  protected Process process;

  protected HibProcessListener () { /* Default constructor for Hibernate */ }

  public HibProcessListener (String type, ExecutionEventType eventType, Process process)
  {
    this.eventType = eventType;
    this.type = type;
    this.process = process;
  }

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  public String getType ()
  {
    return type;
  }

  public void setType (String type)
  {
    this.type = type;
  }

  public ExecutionEventType getEventType ()
  {
    return eventType;
  }

  public void setEventType (ExecutionEventType eventType)
  {
    this.eventType = eventType;
  }

  public Process getProcess ()
  {
    return process;
  }

  public void setProcess (Process process)
  {
    this.process = process;
  }

  @Override
  public int hashCode ()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    return result;
  }

  @Override
  public boolean equals (Object obj)
  {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (!(obj instanceof HibProcessListener))
      return false;
    HibProcessListener other = (HibProcessListener) obj;
    if (id == null)
    {
      if (other.getId() != null)
        return false;
    } else if (!id.equals(other.getId()))
      return false;
    return true;
  }
}
