pragma Extensions_Allowed (On);

with Ada.Text_IO; use Ada.Text_IO;

procedure Proc (B : Boolean) is
   subtype String_N is String ((if B then 1 else 2) .. <>); -- # subtype_decl

   Msg : constant String_N := "hello";           -- # stmt
begin
   Put_Line ("Lower bound: " & Msg'First'Image); -- # stmt
   Put_Line ("Upper bound: " & Msg'Last'Image);  -- # stmt
end Proc;
