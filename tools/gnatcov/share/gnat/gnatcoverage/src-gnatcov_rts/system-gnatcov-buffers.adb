package body System.GNATcov.Buffers is

   -------------
   -- Witness --
   -------------

   procedure Witness (Buffer : in out Coverage_Buffer_Type; Bit : Bit_Id) is
   begin
      Buffer (Bit) := True;
   end Witness;

   function Witness
     (Buffer : in out Coverage_Buffer_Type; Bit : Bit_Id)
      return Witness_Dummy_Type is
   begin
      Witness (Buffer, Bit);
      return (null record);
   end Witness;

end System.GNATcov.Buffers;
