with Basic, Support; use Basic, Support;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_Basic_TF is
   Long : String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   Ulong : Unbounded_String := To_Unbounded_String (Long);
   Short : String := "ABC";
   Ushort : Unbounded_String := To_Unbounded_String (short);
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

   Try1 (Short);
   Assert (Last_Long = Ulong);
   Assert (Last_Short = Ushort);
   Try2 (Short);
   Assert (Last_Long = Ulong);
   Assert (Last_Short = Ushort);
   Try3 (Short);
   Assert (Last_Long = Ulong);
   Assert (Last_Short = Ushort);
end;

--# basic.adb
--  /decl/ l+ ## 0
--  /test/ l+ ## 0
--  /long/ l+ ## 0
--  /short/ l+ ## 0

