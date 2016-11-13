with Basic, Support; use Basic, Support;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_Basic_F is
   Short : String := "AB";
   Ushort : Unbounded_String := To_Unbounded_String (Short);
begin
   Try1 (Short);
   Assert (Last_Long = Null_Unbounded_String);
   Assert (Last_Short = Ushort);
   Try2 (Short);
   Assert (Last_Long = Null_Unbounded_String);
   Assert (Last_Short = Ushort);
   Try3 (Short);
   Assert (Last_Long = Null_Unbounded_String);
   Assert (Last_Short = Ushort);
end;

--# basic.adb
--  /decl/ l+ ## 0
--  /test/ l! ## dT-
--  /long/ l- ## s-
--  /short/ l+ ## 0

