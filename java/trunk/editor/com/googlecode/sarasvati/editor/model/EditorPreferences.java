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

    Copyright 2009 Paul Lorenz
 */
package com.googlecode.sarasvati.editor.model;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import com.googlecode.sarasvati.editor.dialog.DialogFactory;

public class EditorPreferences
{
  private static final EditorPreferences INSTANCE = new EditorPreferences();

  public static EditorPreferences getInstance ()
  {
    return INSTANCE;
  }

  private static final String LIBRARY_PATH_KEY = "libraryPath";
  private static final String RECURSE_LIBRARY_KEY = "recurseLibrary";
  private static final String NODE_TYPES_KEY = "nodeTypes";
  private static final String ATTRIBUTES_KEY = "attributes";

  private static final String NODE_TYPE_NAME = "nodeTypeName";
  private static final String NODE_TYPE_ALLOW_CUSTOM = "nodeTypeAllowCustom";

  private static final String NODE_TYPE_ATTR_NAME = "nodeTypeAttrName";
  private static final String NODE_TYPE_ATTR_DEFAULT_VALUE = "nodeTypeAttrDefaultValue";
  private static final String NODE_TYPE_ATTR_USE_CDATA = "nodeTypeAttrUseCDATA";

  protected String libraryPath;

  protected boolean recurseLibrary;

  protected List<EditorNodeType> nodeTypes;

  public void load ()
  {
    Preferences prefs = Preferences.userNodeForPackage( getClass() );
    libraryPath = prefs.get( LIBRARY_PATH_KEY, null );
    recurseLibrary = prefs.getBoolean( RECURSE_LIBRARY_KEY, false );
  }

  public List<EditorNodeType> getNodeTypes ()
  {
    if ( nodeTypes == null )
    {
      try
      {
        loadNodeTypes ();
      }
      catch ( BackingStoreException bse )
      {
        bse.printStackTrace();
        DialogFactory.showError( "Failed to load preferences: " + bse.getMessage() );
        return Collections.emptyList();
      }
    }

    return nodeTypes;
  }

  public void loadNodeTypes () throws BackingStoreException
  {
    List<EditorNodeType> newNodeTypes = new LinkedList<EditorNodeType>();

    Preferences baseNode = Preferences.userNodeForPackage( getClass() );
    Preferences typesNode = baseNode.node( NODE_TYPES_KEY );

    String[] childrenNames = typesNode.childrenNames();

    if ( childrenNames.length == 0 )
    {
      importDefaultNodeTypes();
    }
    else
    {
      for ( String nodeType : childrenNames )
      {
        newNodeTypes.add( loadNodeType( typesNode.node( nodeType ) ) );
      }
    }

    nodeTypes = newNodeTypes;
  }

  public EditorNodeType loadNodeType (Preferences typeNode) throws BackingStoreException
  {
    String name = typeNode.get( NODE_TYPE_NAME, "<error loading type name>" );
    boolean allowCustom = typeNode.getBoolean( NODE_TYPE_ALLOW_CUSTOM, false );

    List<EditorNodeTypeAttribute> attributes = new LinkedList<EditorNodeTypeAttribute>();

    Preferences attributesNode = typeNode.node( ATTRIBUTES_KEY );
    for ( String child : attributesNode.childrenNames() )
    {
      Preferences attrNode = attributesNode.node( child );
      String attrName = attrNode.get( NODE_TYPE_ATTR_NAME, "<error loading attr>" );
      String defaultValue = attrNode.get( NODE_TYPE_ATTR_DEFAULT_VALUE, null );
      boolean useCDATA = attrNode.getBoolean( NODE_TYPE_ATTR_USE_CDATA, false );
      attributes.add( new EditorNodeTypeAttribute( attrName, defaultValue, useCDATA ) );
    }

    return new EditorNodeType( name, allowCustom, attributes );
  }

  public void importDefaultNodeTypes () throws BackingStoreException
  {
    List<EditorNodeType> newNodeTypes = new LinkedList<EditorNodeType>();
    List<EditorNodeTypeAttribute> emptyAttributes = Collections.emptyList();
    newNodeTypes.add( new EditorNodeType( "node", true, emptyAttributes ) );
    newNodeTypes.add( new EditorNodeType( "wait", true, emptyAttributes ) );

    List<EditorNodeTypeAttribute> attributes = new LinkedList<EditorNodeTypeAttribute>();
    attributes.add( new EditorNodeTypeAttribute( "scriptType", "js", false ) );
    attributes.add( new EditorNodeTypeAttribute( "script", null, true ) );

    newNodeTypes.add( new EditorNodeType( "script", false, attributes ) );
    saveNodeTypes( newNodeTypes );
  }

  public void saveNodeTypes (List<EditorNodeType> newNodeTypes) throws BackingStoreException
  {
    Preferences baseNode = Preferences.userNodeForPackage( getClass() );
    Preferences typesNode = baseNode.node( NODE_TYPES_KEY );
    typesNode.removeNode();

    typesNode = baseNode.node( NODE_TYPES_KEY );

    int count = 1;

    for ( EditorNodeType nodeType : newNodeTypes )
    {
      persistNodeType( typesNode, "nodeType" + count, nodeType );
      count++;
    }
    nodeTypes = newNodeTypes;
  }

  private void persistNodeType (Preferences typesNode, String nodeName, EditorNodeType nodeType)
  {
    Preferences typeNode = typesNode.node( nodeName );
    typeNode.put( NODE_TYPE_NAME, nodeType.getName() );
    typeNode.putBoolean( NODE_TYPE_ALLOW_CUSTOM, nodeType.isAllowNonSpecifiedAttributes() );

    int count = 1;

    Preferences attributesNode = typeNode.node( ATTRIBUTES_KEY );

    for ( EditorNodeTypeAttribute attr : nodeType.getAttributes() )
    {
      Preferences attrNode = attributesNode.node( "attribute" + count );
      attrNode.put( NODE_TYPE_ATTR_NAME, attr.getName() );
      attrNode.put( NODE_TYPE_ATTR_DEFAULT_VALUE, attr.getDefaultValue() );
      attrNode.putBoolean( NODE_TYPE_ATTR_USE_CDATA, attr.isUseCDATA() );
      count++;
    }
  }
}