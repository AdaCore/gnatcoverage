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

   function Witness
     (Buffer              : in out Coverage_Buffer_Type;
      False_Bit, True_Bit : Bit_Id;
      Value               : Boolean) return Boolean
   is
   begin
      if Value then
         Witness (Buffer, True_Bit);
      else
         Witness (Buffer, False_Bit);
      end if;
      return Value;
   end Witness;

end System.GNATcov.Buffers;
