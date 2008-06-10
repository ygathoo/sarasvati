package org.codemonk.wf.xml;

import javax.xml.bind.JAXBException;

public interface XmlWorkflowResolver
{
  XmlWorkflow resolve (String name) throws JAXBException;
}
