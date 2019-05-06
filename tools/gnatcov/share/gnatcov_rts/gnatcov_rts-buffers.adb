--  This unit needs to be compilable with Ada 95 compilers

package body GNATcov_RTS.Buffers is

   subtype Unbounded_Coverage_Buffer_Type is Coverage_Buffer_Type (Bit_Id);

   -------------
   -- Witness --
   -------------

   procedure Witness (Buffer_Address : System.Address; Bit : Bit_Id) is
      Buffer : Unbounded_Coverage_Buffer_Type;
      for Buffer'Address use Buffer_Address;
      pragma Import (Ada, Buffer);
   begin
      Buffer (Bit) := True;
   end Witness;

   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id) return Witness_Dummy_Type
   is
   begin
      Witness (Buffer_Address, Bit);
      return (null record);
   end Witness;

   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id) return Boolean
   is
   begin
      Witness (Buffer_Address, Bit);
      return True;
   end Witness;

   function Witness
     (Buffer_Address      : System.Address;
      False_Bit, True_Bit : Bit_Id;
      Value               : Boolean) return Boolean is
   begin
      if Value then
         Witness (Buffer_Address, True_Bit);
      else
         Witness (Buffer_Address, False_Bit);
      end if;
      return Value;
   end Witness;

   function Witness
     (Decision_Buffer_Address : System.Address;
      False_Bit, True_Bit     : Bit_Id;
      MCDC_Buffer_Address     : System.Address;
      MCDC_Base               : Bit_Id;
      MCDC_Path_Address       : System.Address;
      Value                   : Boolean) return Boolean
   is
      MCDC_Path_Index : Any_Bit_Id;
      for MCDC_Path_Index'Address use MCDC_Path_Address;
      pragma Import (Ada, MCDC_Path_Index);

   begin
      Witness (MCDC_Buffer_Address, MCDC_Base + MCDC_Path_Index);
      return Witness (Decision_Buffer_Address, False_Bit, True_Bit, Value);
   end Witness;

   function Witness
     (Buffer_Address  : System.Address;
      Offset_For_True : Any_Bit_Id;
      First           : Boolean;
      Value           : Boolean) return Boolean
   is
      MCDC_Path_Index : Any_Bit_Id;
      for MCDC_Path_Index'Address use Buffer_Address;
      pragma Import (Ada, MCDC_Path_Index);
   begin
      if First then
         MCDC_Path_Index := 0;
      end if;

      if Value then
         MCDC_Path_Index := MCDC_Path_Index + Offset_For_True;
      end if;

      return Value;
   end Witness;

end GNATcov_RTS.Buffers;
