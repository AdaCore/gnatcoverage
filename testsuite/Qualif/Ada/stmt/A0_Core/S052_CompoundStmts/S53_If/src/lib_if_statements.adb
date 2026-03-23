with Lib_Init;
-- A helper to be provided by each test, to influence which
-- part of the IF statement below gets exercised.

package body Lib_If_Statements is

   Values : array (1 .. 8) of Integer;

   function Probe (Slot : Integer) return Integer is
   begin
      return Values (Slot);
   end;
begin
   if Lib_Init.Init_Ref > 0 then -- # test_if
      Values := (others => 1); -- # do_if
   elsif Lib_Init.Init_Ref = 0 then -- # test_elsif
      Values := (others => 0); -- # do_elsif
   else
      Values := (others => -1); -- # do_else
   end if;
end;
