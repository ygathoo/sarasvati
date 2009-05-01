/**
 * Created on Apr 30, 2009
 */
package com.googlecode.sarasvati.jdbc.dialect;

import com.googlecode.sarasvati.jdbc.JdbcArc;
import com.googlecode.sarasvati.jdbc.JdbcGraph;
import com.googlecode.sarasvati.jdbc.JdbcGraphProcess;
import com.googlecode.sarasvati.jdbc.JdbcNode;
import com.googlecode.sarasvati.jdbc.JdbcNodeRef;
import com.googlecode.sarasvati.jdbc.stmt.AbstractStatement;
import com.googlecode.sarasvati.jdbc.stmt.ArcInsertStatement;
import com.googlecode.sarasvati.jdbc.stmt.GraphInsertStatement;
import com.googlecode.sarasvati.jdbc.stmt.NodeInsertStatement;
import com.googlecode.sarasvati.jdbc.stmt.NodeRefInsertStatement;
import com.googlecode.sarasvati.jdbc.stmt.ProcessInsertStatement;

public class PostgreSQLDatabaseDialect extends AbstractDatabaseDialect
{
  private static final String GRAPH_INSERT_SQL =
    "insert into wf_graph (name, version) values ( ?, ? ) returning id";

  private static final String NODE_INSERT_SQL =
    "insert into wf_node (graph_id, name, type, guard, is_start, is_join) values ( ?, ?, ?, ?, ?, ? ) returning id";

  private static final String NODE_REF_INSERT_SQL =
    "insert into wf_node_ref (graph_id, node_id, instance ) values ( ?, ?, ? ) returning id";

  private static final String ARC_INSERT_SQL =
    "insert into wf_arc (graph_id, a_node_ref_id, z_node_ref_id, name ) values ( ?, ?, ?, ? ) returning id";

  private static final String PROCESS_INSERT_SQL =
    "insert into wf_process (graph_id, state, parent_token_id, create_date, version) values ( ?, 0, ?, ?, 1 ) returning id";


  @Override
  public AbstractStatement newGraphInsertStatement (JdbcGraph graph)
  {
    return new GraphInsertStatement( GRAPH_INSERT_SQL, graph );
  }

  @Override
  public AbstractStatement newNodeInsertStatement (JdbcNode node)
  {
    return new NodeInsertStatement( NODE_INSERT_SQL, node );
  }

  @Override
  public AbstractStatement newArcInsertStatement (JdbcArc arc)
  {
    return new ArcInsertStatement( ARC_INSERT_SQL, arc );
  }

  @Override
  public AbstractStatement newNodeRefInsertStatement (JdbcNodeRef nodeRef)
  {
    return new NodeRefInsertStatement( NODE_REF_INSERT_SQL, nodeRef );
  }

  @Override
  public AbstractStatement newProcessInsertStatement (JdbcGraphProcess process)
  {
    return new ProcessInsertStatement( PROCESS_INSERT_SQL, process );
  }
}