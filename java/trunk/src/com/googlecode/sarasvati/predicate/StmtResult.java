package com.googlecode.sarasvati.predicate;

import java.util.Date;

import com.googlecode.sarasvati.GuardResponse;

public interface StmtResult
{
  Object getResult ();
  String getString ();
  Integer getInt ();
  Date getDate ();
  GuardResponse getGuardResponse ();

  StmtResult getType ();
}
