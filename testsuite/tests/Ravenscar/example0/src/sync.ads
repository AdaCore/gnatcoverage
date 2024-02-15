with System;

package Sync is

  -- Simple 1 slot protected buffer

  protected type Buffer is
    procedure Push (V : Integer; Tell : Boolean);
    entry Pop (V : out Integer; Tell : Boolean);
  private
    Value : Integer;
    Has_Value : Boolean := False;
  end;

  -- One producer and one consumer of Buffer data.
  -- See the Notes at the end for the choice of priorities.

  task Producer is
     pragma Priority (System.Default_Priority-2);
  end;

  task Consumer is
     pragma Priority (System.Default_Priority-1);
  end;

  -- Simple environment termination control

  protected type Sync_Point (N : Positive) is
    procedure Reach;
    entry Wait; -- until N calls to Reach
  private
    Reaches_So_Far : Natural := 0;
    Pass : Boolean := False;
  end;

  -- To let the environment task know when both
  -- the Consumer and the Producer tasks are done

  Termination : Sync_Point (N => 2);

  -- Notes: the priorties are chosen to have
  --
  --    prio (environment) > prio (consumer) > prio (producer)
  --
  -- to make sure that the guards are exercised both ways, False
  -- then True.

end;
