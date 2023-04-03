package body Ctxt is
   pragma Unsuppress (All_Checks);

   procedure Run (Iraise, Xraise : Boolean) is
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
end;
