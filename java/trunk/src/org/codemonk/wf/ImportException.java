package org.codemonk.wf;

public class ImportException extends Exception
{
  private static final long serialVersionUID = 1L;

  public ImportException (String message, Throwable cause)
  {
    super( message, cause );
  }

  public ImportException( String message )
  {
    super( message );
  }
}
