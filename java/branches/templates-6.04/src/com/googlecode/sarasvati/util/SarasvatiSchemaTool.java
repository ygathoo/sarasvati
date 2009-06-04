/**
 * Created on May 19, 2009
 */
package com.googlecode.sarasvati.util;

import java.io.File;
import java.sql.SQLException;
import java.sql.Statement;

import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.cfg.Settings;
import org.hibernate.classic.Session;

import com.googlecode.sarasvati.hib.HibEngine;

public class SarasvatiSchemaTool
{
  private AnnotationConfiguration config = new AnnotationConfiguration();

  public SarasvatiSchemaTool (String hibernateCfg)
  {
    HibEngine.addToConfiguration( config, true );
    config.configure( new File( hibernateCfg ) );
  }

  public String[] generateCreateSchemaDDL ()
  {
    Settings settings = config.buildSettings();
    return config.generateSchemaCreationScript( settings.getDialect() );
  }

  public String[] generateDropSchemaDDL ()
  {
    Settings settings = config.buildSettings();
    return config.generateDropSchemaScript( settings.getDialect() );
  }

  public void executeDDL (String[] ddl) throws Exception
  {
    SessionFactory factory = config.buildSessionFactory();

    Session session = factory.openSession();
    try
    {
      session.getTransaction().begin();
      executeDDL( session, ddl );
      session.getTransaction().commit();
    }
    catch ( Exception e )
    {
      session.getTransaction().rollback();
      throw e;
    }
    finally
    {
      session.close();
    }
  }

  @SuppressWarnings("deprecation")
  public void executeDDL (Session session, String[] ddl)
    throws SQLException
  {
    Statement stmt = session.connection().createStatement();

    try
    {
      for ( String ddlStmt : ddl )
      {
        System.out.println( "DDL: " + ddlStmt );
        stmt.execute( ddlStmt );
      }
    }
    finally
    {
      stmt.close();
    }
  }

  public void createSchema () throws Exception
  {
    executeDDL( generateCreateSchemaDDL() );
  }

  public void dropSchema () throws Exception
  {
    executeDDL( generateDropSchemaDDL() );
  }

  public static void main (String[] args) throws Exception
  {
    SarasvatiSchemaTool createSchema = new SarasvatiSchemaTool( "/home/paul/workspace/wf-java/conf/hibernate.cfg.xml" );
    // createSchema.dropSchema();
    createSchema.createSchema();
  }
}