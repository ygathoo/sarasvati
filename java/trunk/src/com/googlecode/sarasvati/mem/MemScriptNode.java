/*
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
*/
package com.googlecode.sarasvati.mem;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.WorkflowException;

public class MemScriptNode extends MemNode
{
  protected String script;
  protected String scriptType;

  public String getScript ()
  {
    return script;
  }

  public void setScript (String script)
  {
    this.script = script;
  }

  public String getScriptType ()
  {
    return scriptType;
  }

  public void setScriptType (String scriptType)
  {
    this.scriptType = scriptType;
  }

  @Override
  public void execute (Engine engine, NodeToken token)
  {
    if ( scriptType == null || scriptType.trim().isEmpty() )
    {
      scriptType = "js";
    }

    try
    {
      ScriptEngine scriptEngine = new ScriptEngineManager().getEngineByExtension( scriptType );

      if ( scriptEngine == null )
      {
        throw new WorkflowException( "No script engine found for type '" + scriptType + ". Searched by file extension (i.e. js for javascript, rb for ruby, etc..)" );
      }

      scriptEngine.put( "engine", engine );
      scriptEngine.put( "token", token );

      scriptEngine.eval( script );
    }
    catch ( ScriptException se )
    {
      throw new WorkflowException( "Script of type " + scriptType + " failed to execute. " +
                                   "Script content: " + script, se );
    }

    if ( !token.isComplete() )
    {
      engine.completeExecution( token, Arc.DEFAULT_ARC );
    }
  }
}
