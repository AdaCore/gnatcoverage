with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Strict_Head;
procedure Show_Whether_Local (Var_Name : String) is
   use Ada.Exceptions;

   function Is_Local_Var (S : Unbounded_String) return Boolean is
      R : Boolean;
   begin
      if To_String (Strict_Head (S, 2)) = "_L" then -- # evalA
         R := True;                                 -- # true
      else
         R := False;                                -- # false
      end if;
      return R;                                     -- # ret
   exception
      when E : others =>
         declare
            Name : constant String := Exception_Name (E);          -- # exc
            Msg  : constant String := Exception_Message (E);       -- # exc
         begin
            Put_Line ("Got exception: " & Name & "(" & Msg & ")"); -- # exc
         end;
         raise;                                                    -- # exc
   end Is_Local_Var;

begin
   if Is_Local_Var (To_Unbounded_String (Var_Name)) then -- # evalD
      Put_Line ("local");                                -- # true
   end if;
end Show_Whether_Local;
