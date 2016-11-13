with Basic, Support; use Basic, Support;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_Basic_T is
   Long : String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   Ulong : Unbounded_String := To_Unbounded_String (Long);
begin
   Try1 (Long);
   Assert (Last_Long = Ulong);
   Assert (Last_Short = Null_Unbounded_String);
   Try2 (Long);
   Assert (Last_Long = Ulong);
   Assert (Last_Short = Null_Unbounded_String);
   Try3 (Long);
   Assert (Last_Long = Ulong);
   Assert (Last_Short = Null_Unbounded_String);
end;

--# basic.adb
--  /decl/ l+ ## 0
--  /test/ l! ## dF-
--  /long/ l+ ## 0
--  /short/ l- ## s-

