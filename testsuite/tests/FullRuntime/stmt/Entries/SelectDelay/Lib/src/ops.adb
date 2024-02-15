with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;   use Ada.Text_IO;

package body Ops is

   task type Opmaster is
      entry Start (O : Opkind);
      entry Compute (A, B : Boolean; Result : out Boolean);
   end Opmaster;

   task body Opmaster is
      Op : Opkind;
   begin
      accept Start (O : Opkind) do  -- # compute
         Op := O; -- # compute
      end Start;

      select -- # compute
         accept Compute (A, B : Boolean; Result : out Boolean) do -- # compute
            case Op is -- # compute
               when Op_And => Result := A and then B; -- # do_and
               when Op_Or  => Result := A or else B;  -- # do_or
            end case;

         end;
      or
         delay To_Duration (Minutes (1));
      end select;
   end;

   function Compute (Op : Opkind; A, B : Boolean) return Boolean is

      T : Opmaster;
      Result : Boolean;
   begin
      select
         T.Compute (A, B, Result); -- # compute
      or
         delay until Clock + Seconds (1); -- # compute
         Put_Line ("T.Compute timed out"); -- # sometimes-timeout
      end select;

      T.Start (Op); -- # compute

      select
         T.Compute (A, B, Result);  -- # compute
      or
         delay until Clock + Minutes (1);
         Put_Line ("T.Compute timed out"); -- # never-timeout
      end select;

      return Result; -- # compute
   end;

end;
