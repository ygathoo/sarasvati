package com.googlecode.sarasvati.predicate;

import java.util.Date;

import com.googlecode.sarasvati.GuardResponse;

public interface StmtResult
{
  String getString ();
  int getInt ();
  Date getDate ();
  GuardResponse getGuardResponse ();

  StmtResult getType ();
}
