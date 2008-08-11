/*
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
*/
/**
 * Created on Apr 25, 2008
 */
package com.googlecode.sarasvati.hib;

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import org.hibernate.annotations.CollectionOfElements;

import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.GuardAction;
import com.googlecode.sarasvati.NestedEnv;
import com.googlecode.sarasvati.NodeToken;

@Entity
@Table(name="wf_node_token")
public class HibNodeToken implements NodeToken
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long    id;

  @ManyToOne(fetch = FetchType.EAGER)
  @JoinColumn(name = "process_id")
  protected HibProcess process;

  @ManyToOne(fetch = FetchType.EAGER)
  @JoinColumn(name = "node_ref_id")
  protected HibNodeRef nodeRef;

  @ManyToOne(fetch = FetchType.EAGER)
  @JoinColumn(name = "attr_set_id")
  protected HibNodeToken attrSetToken;

  @CollectionOfElements
  @JoinTable( name="wf_token_attr", joinColumns={@JoinColumn( name="attr_set_id")})
  @org.hibernate.annotations.MapKey( columns={@Column(name="name")})
  @Column( name="value")
  protected Map<String, String> attrMap;

  @ManyToMany (fetch=FetchType.LAZY, cascade= {CascadeType.ALL})
  @JoinTable( name = "wf_node_token_parent",
              joinColumns = @JoinColumn(name = "node_token_id"),
              inverseJoinColumns = @JoinColumn(name = "arc_token_id") )
  protected List<HibArcToken> parentTokens;

  @Temporal(TemporalType.TIMESTAMP)
  @Column (name="create_date", updatable = false)
  protected Date    createDate;

  @Temporal(TemporalType.TIMESTAMP)
  @Column (name="complete_date")
  protected Date    completeDate;

  @Column (name="guard_action")
  protected GuardAction guardAction;

  @Transient
  protected Map<String,Object> transientAttributes = new HashMap<String, Object>();

  @Transient
  protected Env env = null;

  @Transient
  protected Env fullEnv = null;

  public HibNodeToken () { /* Default constructor for Hibernate */ }

  public HibNodeToken (HibProcess process,
                       HibNodeRef nodeRef,
                       HibNodeToken attrSetToken,
                       Map<String,String> attrMap,
                       List<HibArcToken> parentTokens,
                       Map<String,Object> transientAttributes)
  {
    this.process      = process;
    this.nodeRef      = nodeRef;
    this.attrSetToken = attrMap.isEmpty() ? attrSetToken : this;
    this.attrMap      = attrMap;
    this.parentTokens = parentTokens;
    this.createDate   = new Date();

    this.transientAttributes = transientAttributes;
  }

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  public HibProcess getProcess ()
  {
    return process;
  }

  public void setProcess (HibProcess process)
  {
    this.process = process;
  }

  @Override
  public HibNodeRef getNode ()
  {
    return nodeRef;
  }

  public void setNodeRef (HibNodeRef nodeRef)
  {
    this.nodeRef = nodeRef;
  }

  @Override
  public GuardAction getGuardAction ()
  {
    return guardAction;
  }

  @Override
  public void recordGuardAction (Engine engine, GuardAction action)
  {
    this.guardAction = action;
  }

  public HibNodeToken getAttrSetToken()
  {
    return attrSetToken;
  }

  public void setAttrSetToken( HibNodeToken attrSetToken )
  {
    this.attrSetToken = attrSetToken;
  }

  public Map<String, String> getAttrMap()
  {
    return attrMap;
  }

  public void setAttrMap( Map<String, String> attrMap )
  {
    this.attrMap = attrMap;
  }

  public List<HibArcToken> getParentTokens ()
  {
    return parentTokens;
  }

  public void setParentTokens (List<HibArcToken> parentTokens)
  {
    this.parentTokens = parentTokens;
  }

  public Date getCreateDate ()
  {
    return createDate;
  }

  public void setCreateDate (Date createDate)
  {
    this.createDate = createDate;
  }

  public Date getCompleteDate ()
  {
    return completeDate;
  }

  public void setCompleteDate (Date completeDate)
  {
    this.completeDate = completeDate;
  }

  @Override
  public void markComplete (Engine engine)
  {
    this.completeDate = new Date();
  }

  @Override
  public Env getFullEnv()
  {
    if ( fullEnv == null )
    {
      fullEnv = new NestedEnv( env, process.getEnv() );
    }
    return fullEnv;
  }

  @Override
  public Env getEnv()
  {
    if ( env == null )
    {
      env = new HibTokenEnv();
    }
    return env;
  }

  private class HibTokenEnv implements Env
  {
    public HibTokenEnv()
    {
      // Default constructor
    }

    private boolean isAttributeSetLocal ()
    {
      return HibNodeToken.this.equals( attrSetToken );
    }

    @Override
    public String getStringAttribute( String name )
    {
      if ( attrSetToken == null )
      {
        return null;
      }
      else if ( !isAttributeSetLocal() )
      {
        return attrSetToken.getEnv().getStringAttribute( name );
      }
      else
      {
        return attrMap.get( name );
      }
    }

    protected void copyOnWrite ()
    {
      if ( !isAttributeSetLocal() )
      {
        if ( attrSetToken != null )
        {
          attrMap.putAll( attrSetToken.getAttrMap() );
        }
        attrSetToken = HibNodeToken.this;
      }
    }

    @Override
    public void removeAttribute( String name )
    {
      copyOnWrite();
      attrMap.remove( name );
    }

    @Override
    public void setStringAttribute( String name, String value )
    {
      copyOnWrite();
      attrMap.put( name, value );
    }

    @Override
    public boolean getBooleanAttribute( String name )
    {
      return "true".equals( getStringAttribute( name ) );
    }

    @Override
    public long getLongAttribute( String name )
    {
      String value = getStringAttribute( name );

      if ( value == null )
      {
        return 0;
      }

      try
      {
        return Long.parseLong( value );
      }
      catch (NumberFormatException nfe )
      {
        return 0;
      }
    }

    @Override
    public void setBooleanAttribute( String name, boolean value )
    {
      setStringAttribute( name, String.valueOf( value ) );
    }

    @Override
    public void setLongAttribute( String name, long value )
    {
      setStringAttribute( name, String.valueOf( value ) );
    }

    @Override
    public boolean hasAttribute( String name )
    {
      if ( attrSetToken == null )
      {
        return false;
      }
      else if ( !isAttributeSetLocal() )
      {
        return attrSetToken.getEnv().hasAttribute( name );
      }
      else
      {
        return attrMap.containsKey( name );
      }
    }

    @Override
    public Iterable<String> getAttributeNames()
    {
      if ( attrSetToken == null )
      {
        return Collections.emptyList();
      }
      else if ( !isAttributeSetLocal() )
      {
        return attrSetToken.getEnv().getAttributeNames();
      }
      else
      {
        return attrMap.keySet();
      }
    }

    @Override
    public void setTransientAttribute (String name, Object value)
    {
      transientAttributes.put( name, value );
    }

    @Override
    public Object getTransientAttribute (String name)
    {
      return transientAttributes.get( name );
    }

    @Override
    public boolean hasTransientAttribute (String name)
    {
      return transientAttributes.containsKey( name );
    }

    @Override
    public void removeTransientAttribute (String name)
    {
      transientAttributes.remove( name );
    }

    @Override
    public Iterable<String> getTransientAttributeNames()
    {
      return transientAttributes.keySet();
    }

    @Override
    public void importEnv(Env copyEnv)
    {
      for ( String name : copyEnv.getAttributeNames() )
      {
        setStringAttribute( name, copyEnv.getStringAttribute( name ) );
      }

      for ( String name : copyEnv.getTransientAttributeNames() )
      {
        setTransientAttribute( name, copyEnv.getTransientAttribute( name ) );
      }
    }
  }

  @Override
  public int hashCode ()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( id == null )
        ? 0 : id.hashCode() );
    return result;
  }

  @Override
  public boolean equals (Object obj)
  {
    if ( this == obj ) return true;
    if ( obj == null ) return false;
    if ( !( obj instanceof HibNodeToken ) ) return false;
    final HibNodeToken other = (HibNodeToken)obj;
    if ( id == null )
    {
      if ( other.getId() != null ) return false;
    }
    else if ( !id.equals( other.getId() ) ) return false;
    return true;
  }
}