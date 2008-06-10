package org.codemonk.wf;

import org.codemonk.wf.xml.XmlWorkflow;

public interface WfLoader<G extends WfGraph,N extends Node>
{
  void importDefinition (XmlWorkflow definition) throws ImportException;
  
  boolean isLoaded (String name);
}
