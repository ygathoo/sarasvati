package com.googlecode.sarasvati.example;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.CustomNode;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.load.DefaultNodeFactory;
import com.googlecode.sarasvati.mem.MemEngine;

public class HelloNode extends CustomNode
{
  @Override
  public void execute (Engine engine, NodeToken token)
  {
    System.out.println( "Hello, World!" );
    engine.completeExecution( token, Arc.DEFAULT_ARC );
  }
  
  public static void main (String[] args)
  {
    MemEngine engine = new MemEngine ();
    engine.addGlobalCustomNodeType( "helloWorld", HelloNode.class );
    DefaultNodeFactory.addGlobalCustomType( "helloWorld", HelloNode.class );
  }
}
