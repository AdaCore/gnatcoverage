with External_Variable;

package body Decisions is

   type Bool_Array_Range is mod 5;
   subtype Bool_Array_R is Positive range 1 .. 5;
   type Bool_Array is array (Bool_Array_Range) of Boolean;

   BA : Bool_Array := Bool_Array'(others => True);

   function F1 (B1 : Boolean; I, Max : Positive) return Boolean is
   begin
      return B1 or else I > Max; -- # F1
   end F1;

   function F2 (B1 : Boolean; Index : Bool_Array_Range) return Boolean is
   begin
      BA (Index + 1) := B1;           -- # F2
      return B1 and then BA (Index);  -- # F2
   end F2;

   function F3 (X : Positive) return Positive is
   begin
      if X mod Integer (Bool_Array_Range'Last) > 3 then -- # F3if
         return X; -- # F3true
      end if;
      return X - 1; -- # F3false
   end F3;

   procedure FunctionCalls_And_Operators
     (I1, I2, I3, Max : Positive;
      Result : out Boolean) is
         I1_Local : Positive := I1;  -- # Idecl
         I2_Local : Positive := I2;  -- # Idecl
         I3_Local : Positive := I3;  -- # Idecl
   begin


      while (I1_Local < Max or else I2_Local < Max) -- # whileOr
        and then F3 (I3_Local) < Max loop -- # whileAnd

         if  I1_Local in Bool_Array_R then  -- # if
            C1 := F2 (C2, Bool_Array_Range (I1_Local)); -- # evalIfTrue
         end if;

         I1_Local := I1_Local - 1;  -- # whileBody
         I2_Local := I2_Local - 1;  -- # whileBody
         I3_Local := I3_Local - 1;  -- # whileBody

         C2 := F1 (C2, I1_Local, Max);  -- # whileBody

      end loop;

      Result := C1 and then C2; -- # andthen

   end FunctionCalls_And_Operators;

begin

   if External_Variable.State then  -- # checkState
      C2 := True;                            -- # stateTrue
   end if;

end Decisions;
