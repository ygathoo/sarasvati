package com.googlecode.sarasvati.example.db;

import org.hibernate.Session;

import com.googlecode.sarasvati.NodeToken;

public class TaskDAO
{
  public static Task getTaskForToken (Session session, NodeToken token)
  {
    String query = "from Task where nodeToken = :token";
    return (Task)session.createQuery( query ).setEntity( "token", token ).uniqueResult();
  }
}
