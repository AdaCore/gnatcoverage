with Support;

package Odecls is
   type Byte is mod 2 ** 8;
   type Sequence is array (Natural range <>) of Byte;

   type Msg (Len : Natural) is record
      Valid : Boolean := True;
      Data  : Sequence (1 .. Len) := (others => 16#EE#);
   end record;

   Static_Int : Integer := 12; -- # stmt
   Latch_Int  : Integer := Support.Identity (Static_Int); -- # stmt

   Global_Msg : Msg (Len => 16); -- # stmt

   procedure Check_Locals;
   --  Straightline subprogram with "# code" marks on every stmt
   --  expected to generate code, on object declarations as well.
end;
