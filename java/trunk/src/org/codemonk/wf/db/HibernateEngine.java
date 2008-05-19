/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import java.util.List;

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
    NodeToken token = new NodeToken( (Process)process, (NodeRef)node, (List<ArcToken>)(List<?>)parents );
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