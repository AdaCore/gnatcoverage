package body System.GNATcov.Buffers is

   subtype Unbounded_Coverage_Buffer_Type is Coverage_Buffer_Type (Bit_Id);

   -------------
   -- Witness --
   -------------

   procedure Witness (Buffer : in out Coverage_Buffer_Type; Bit : Bit_Id) is
   begin
      Buffer (Bit) := True;
   end Witness;

   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id)
      return Witness_Dummy_Type
   is
      Buffer : Unbounded_Coverage_Buffer_Type;
      for Buffer'Address use Buffer_Address;
   begin
      Witness (Buffer, Bit);
      return (null record);
   end Witness;

   function Witness
     (Buffer_Address      : System.Address;
      False_Bit, True_Bit : Bit_Id;
      Value               : Boolean) return Boolean
   is
      Buffer : Unbounded_Coverage_Buffer_Type;
      for Buffer'Address use Buffer_Address;
   begin
      if Value then
         Witness (Buffer, True_Bit);
      else
         Witness (Buffer, False_Bit);
      end if;
      return Value;
   end Witness;

end System.GNATcov.Buffers;
