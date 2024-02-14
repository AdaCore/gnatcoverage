pragma Unsuppress (All_Checks);

package body Ctxt is

   generic
      Iraise : Boolean;
      Xraise : Boolean;
   package G_Checker is
   end;

   package body G_Checker is
   begin
      declare
        procedure Check (Iraise, Xraise : Boolean) is
        begin
          X := X + 1; -- # pre
          if Iraise then -- # ieval
             R0 := I1; -- # iraise
          elsif Xraise then -- # xeval
             raise Constraint_Error; -- # xraise
          end if;
          X := X + 1; -- # post
        exception
          when Constraint_Error =>
            Handled := Handled + 1; -- # handler
        end;
     begin
      Check (Iraise => Iraise, Xraise => Xraise); -- # pre
     end;
   end;

   procedure Run (Iraise, Xraise : Boolean) is
      package Checker is
         new G_Checker (Iraise => Iraise, Xraise => Xraise);
   begin
      null; -- # pre
   end;
end;
