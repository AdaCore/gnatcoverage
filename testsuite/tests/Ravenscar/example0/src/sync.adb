with GNAT.IO; use GNAT.IO;
package body Sync is

  protected body Buffer is
    procedure Push (V : Integer; Tell : Boolean) is
    begin
      if Tell then                                -- # psh_ttell
         Put_Line ("push " & Integer'Image (V));  -- # psh_tell
      end if;
      Value := V;        -- # psh_do
      Has_Value := True; -- # psh_do
    end;

    entry Pop (V : out Integer; Tell : Boolean) when Has_Value is
    begin
      V := Value;  -- # pop_do
      if Tell then -- # pop_ttell
         Put_Line ("pop " & Integer'Image (V)); -- # pop_tell
      end if;
      Has_Value := False; -- # pop_do
    end;
  end Buffer;

  protected body Sync_Point is
    procedure Reach is
    begin
      Reaches_So_Far := Reaches_So_Far + 1; -- # rch_do
      Pass := Reaches_So_Far >= N;          -- # rch_do
    end;

    entry Wait when Pass is -- # wat_guard
    begin
       null; -- # wat_do
    end;
  end Sync_Point;

  --

  B0 : Buffer;

  task body Producer is
  begin
    B0.Push (V => 12, Tell => False); -- # pro_do
    Termination.Reach;                -- # pro_do
  end;

  task body Consumer is
     V : Integer;
  begin
    B0.Pop (V => V, Tell => True);  -- # con_do
    Termination.Reach;              -- # con_do
  end;
end;
