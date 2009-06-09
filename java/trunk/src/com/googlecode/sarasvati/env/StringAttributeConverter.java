package com.googlecode.sarasvati.env;

/**
 * Identity converter for String <--> String
 *
 * @author Paul Lorenz
 */
public final class StringAttributeConverter implements AttributeConverter
{
  @Override
  public String objectToString (Object object)
  {
    return (String)object;
  }

  @Override
  public Object stringToObject (String string, Class<?> object)
  {
    return string;
  }
}