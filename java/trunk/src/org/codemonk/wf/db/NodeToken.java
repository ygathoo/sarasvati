/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import java.util.Date;
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

import org.codemonk.wf.INodeToken;
import org.hibernate.annotations.CollectionOfElements;

@Entity
@Table(name="wf_node_token")
public class NodeToken implements INodeToken
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long    id;

  @ManyToOne(fetch = FetchType.EAGER)
  @JoinColumn(name = "process_id")
  protected Process process;

  @ManyToOne(fetch = FetchType.EAGER)
  @JoinColumn(name = "node_ref_id")
  protected NodeRef nodeRef;

  @ManyToOne(fetch = FetchType.EAGER)
  @JoinColumn(name = "attr_set_id")
  protected NodeToken attrSetToken;

  @CollectionOfElements
  @JoinTable( name="wf_token_string_attr", joinColumns={@JoinColumn( name="attr_set_id")})
  @org.hibernate.annotations.MapKey( columns={@Column(name="name")})
  @Column( name="value")
  protected Map<String, String> attrMap;

  @ManyToMany (fetch=FetchType.LAZY, cascade= {CascadeType.ALL})
  @JoinTable( name = "wf_node_token_parent",
              joinColumns = @JoinColumn(name = "node_token_id"),
              inverseJoinColumns = @JoinColumn(name = "arc_token_id") )
  protected List<ArcToken> parentTokens;

  @Temporal(TemporalType.TIMESTAMP)
  @Column (name="create_date", updatable = false)
  protected Date    createDate;

  @Temporal(TemporalType.TIMESTAMP)
  @Column (name="complete_date")
  protected Date    completeDate;

  public NodeToken () { /* Default constructor for Hibernate */ }

  public NodeToken (Process process, NodeRef nodeRef, NodeToken attrSetToken, Map<String,String> attrMap, List<ArcToken> parentTokens)
  {
    this.process      = process;
    this.nodeRef      = nodeRef;
    this.attrSetToken = attrMap.isEmpty() ? attrSetToken : this;
    this.attrMap      = attrMap;
    this.parentTokens = parentTokens;
    this.createDate   = new Date();
  }

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
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
  public NodeRef getNode ()
  {
    return nodeRef;
  }

  public void setNodeRef (NodeRef nodeRef)
  {
    this.nodeRef = nodeRef;
  }

  public NodeToken getAttrSetToken()
  {
    return attrSetToken;
  }

  public void setAttrSetToken( NodeToken attrSetToken )
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

  public List<ArcToken> getParentTokens ()
  {
    return parentTokens;
  }

  public void setParentTokens (List<ArcToken> parentTokens)
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
  public void markComplete ()
  {
    this.completeDate = new Date();
  }

  @Override
  public String getStringAttribute( String name )
  {
    if ( attrSetToken == null )
    {
      return null;
    }
    else if ( !this.equals( attrSetToken ) )
    {
      return attrSetToken.getStringAttribute( name );
    }
    else
    {
      return attrMap.get( name );
    }
  }

  protected void copyOnWrite ()
  {
    if ( !this.equals( attrSetToken ) )
    {
      if ( attrSetToken != null )
      {
        attrMap.putAll( attrSetToken.getAttrMap() );
      }
      attrSetToken = this;
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
    if ( !( obj instanceof NodeToken ) ) return false;
    final NodeToken other = (NodeToken)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}