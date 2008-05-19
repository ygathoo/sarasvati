/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codemonk.wf.Engine;
import org.codemonk.wf.IArc;
import org.codemonk.wf.IArcToken;
import org.codemonk.wf.IGraph;
import org.codemonk.wf.INode;
import org.codemonk.wf.INodeToken;
import org.codemonk.wf.IProcess;
import org.hibernate.Session;

public class HibernateEngine extends Engine
{
  protected Session session;

  public HibernateEngine (Session session)
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
  protected ArcToken newArcToken (IProcess process, IArc arc, INodeToken previousToken)
  {
    ArcToken token = new ArcToken( (Process)process, (Arc)arc, (NodeToken)previousToken );
    session.save( token );
    return token;
  }

  @SuppressWarnings("unchecked")
  @Override
  protected NodeToken newNodeToken (IProcess process, INode node, List<IArcToken> parents)
  {
    // Here we setup the token attributes for the new node
    // If the node has no precessors, it will have no attributes
    // If it has only one processor (or only one processor with attributes)
    // it will inherit the attributes of that one node
    // Otherwise, the attributes of all precessor nodes will get merged into
    // a single set.
    List<ArcToken> hibParents = (List<ArcToken>)(List<?>)parents;

    NodeToken attrSetToken = null;
    Map<String,String> attrMap = new HashMap<String,String>();

    boolean isMerge = false;

    for ( ArcToken arcToken : hibParents )
    {
      NodeToken parent = arcToken.getParentToken();

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

    NodeToken token = new NodeToken( (Process)process, (NodeRef)node, attrSetToken, attrMap, hibParents);
    session.save( token );
    return token;
  }

  @Override
  protected Process newProcess( IGraph graph )
  {
    Process process = new Process( (Graph)graph);
    session.save(  process );

    return process;
  }

  @SuppressWarnings("unchecked")
  public List<Graph> getGraphs ()
  {
    return session.createQuery( "from Graph" ).list();
  }
}