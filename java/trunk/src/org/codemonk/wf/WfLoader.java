package org.codemonk.wf;

import org.codemonk.wf.xml.XmlWorkflow;

public interface WfLoader
{
  void importDefinition (XmlWorkflow definition) throws ImportException;
}
