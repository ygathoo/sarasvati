/**
 * Created on Apr 30, 2009
 */
package com.googlecode.sarasvati.example.jdbc;

import com.googlecode.sarasvati.jdbc.stmt.AbstractStatement;

public interface ExampleStatementFactory
{
  AbstractStatement newInsertTaskNodeStatement (JdbcExampleTaskNode taskNode);
}
