/**
 * Created on Jun 12, 2008
 */
package com.googlecode.sarasvati.example.mem;

import java.util.LinkedList;
import java.util.List;

public class TaskList
{
  protected static List<Task> tasks = new LinkedList<Task>();

  public static List<Task> getTasks ()
  {
    return tasks;
  }
}
