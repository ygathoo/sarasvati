/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codemonk.wf.Engine;
import org.codemonk.wf.Arc;
import org.codemonk.wf.ArcToken;
import org.codemonk.wf.Graph;
import org.codemonk.wf.Node;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.Process;
import org.hibernate.Session;

public class HibEngine extends Engine
{
  protected Session session;

  public HibEngine (Session session)
  {
    this.session = session;
  }

  public Session getSession ()
  {
    return session;
  }

  public void setSession (Session session)
  {
    this.session = session;
  }

  @Override
  protected HibArcToken newArcToken (Process process, Arc arc, NodeToken previousToken)
  {
    HibArcToken token = new HibArcToken( (HibProcess)process, (HibArc)arc, (HibNodeToken)previousToken );
    session.save( token );
    return token;
  }

  @SuppressWarnings("unchecked")
  @Override
  protected HibNodeToken newNodeToken (Process process, Node node, List<ArcToken> parents)
  {
    // Here we setup the token attributes for the new node
    // If the node has no predecessors, it will have no attributes
    // If it has only one processor (or only one processor with attributes)
    // it will inherit the attributes of that one node
    // Otherwise, the attributes of all predecessor nodes will get merged into
    // a single set.
    List<HibArcToken> hibParents = (List<HibArcToken>)(List<?>)parents;

    HibNodeToken attrSetToken = null;
    Map<String,String> attrMap = new HashMap<String,String>();

    boolean isMerge = false;

    for ( HibArcToken arcToken : hibParents )
    {
      HibNodeToken parent = arcToken.getParentToken();

      if ( parent.getAttrSetToken() == null )
      {
        continue;
      }
      if ( attrSetToken == null )
      {
        attrSetToken = parent.getAttrSetToken();
      }
      else if ( !isMerge )
      {
        attrMap.putAll( attrSetToken.getAttrMap() );
        isMerge = true;
      }

      if ( isMerge )
      {
        attrMap.putAll( parent.getAttrMap() );
      }
    }

    HibNodeToken token = new HibNodeToken( (HibProcess)process, (HibNodeRef)node, attrSetToken, attrMap, hibParents);
    session.save( token );
    return token;
  }

  @Override
  protected HibProcess newProcess( Graph graph )
  {
    HibProcess process = new HibProcess( (HibGraph)graph);
    session.save(  process );

    return process;
  }

  @SuppressWarnings("unchecked")
  public List<HibGraph> getGraphs ()
  {
    return session.createQuery( "from HibGraph" ).list();
  }
}