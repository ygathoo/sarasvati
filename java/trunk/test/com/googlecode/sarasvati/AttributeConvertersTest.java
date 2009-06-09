/**
 * Created on Jun 9, 2009
 */
package com.googlecode.sarasvati;

import junit.framework.Assert;

import com.googlecode.sarasvati.impl.MapEnv;

import org.junit.Test;

public class AttributeConvertersTest
{
  @Test public void testByte ()
  {
    MapEnv env = new MapEnv ();
    Byte test = 1;
    env.setAttribute( "test", test );
    Assert.assertEquals( test, env.getAttribute( "test", Byte.class ) );
  }

  @Test public void testBoolean ()
  {
    MapEnv env = new MapEnv ();
    Boolean test = Boolean.TRUE;
    env.setAttribute( "test", test );
    Assert.assertEquals( test, env.getAttribute( "test", Boolean.class ) );
  }

  @Test public void testShort ()
  {
    MapEnv env = new MapEnv ();
    Short test = 1;
    env.setAttribute( "test", test );
    Assert.assertEquals( test, env.getAttribute( "test", Short.class ) );
  }

  @Test public void testCharacter ()
  {
    MapEnv env = new MapEnv ();
    Character test = 1;
    env.setAttribute( "test", test );
    Assert.assertEquals( test, env.getAttribute( "test", Character.class ) );
  }

  @Test public void testInteger ()
  {
    MapEnv env = new MapEnv ();
    Integer test = 1;
    env.setAttribute( "test", test );
    Assert.assertEquals( test, env.getAttribute( "test", Integer.class ) );
  }

  @Test public void testFloat ()
  {
    MapEnv env = new MapEnv ();
    Float test = 1f;
    env.setAttribute( "test", test );
    Assert.assertEquals( test, env.getAttribute( "test", Float.class ) );
  }

  @Test public void testLong ()
  {
    MapEnv env = new MapEnv ();
    Long test = 1l;
    env.setAttribute( "test", test );
    Assert.assertEquals( test, env.getAttribute( "test", Long.class ) );
  }

  @Test public void testDouble ()
  {
    MapEnv env = new MapEnv ();
    Double test = 1d;
    env.setAttribute( "test", test );
    Assert.assertEquals( test, env.getAttribute( "test", Double.class ) );
  }
}