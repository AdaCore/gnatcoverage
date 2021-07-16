------------------------------------------------------------------------------
--                                                                          --
--                            GNATcoverage                                  --
--                                                                          --
--                    Copyright (C) 2008-2021, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

package body Foo
  with Refined_State => (Data => (Data_Array, Last_Increment))
is
   Last_Increment : Integer := 0;
   Data_Array     : array (1 .. 4) of Integer := (others => 0);

   procedure Process_1 (Value : Integer)
     with Global => (Output => (Data_Array, Last_Increment)), Pre => Value > 0;

   procedure Process (Value : Integer) is
   begin
      Process_1 (Value => Value);
   end Process;

   procedure Process_1 (Value : Integer) is
   begin
      for I in Data_Array'Range loop
         Data_Array (I) := Data_Array (I) + Value;

         pragma Loop_Invariant
           (for all J in Data_Array'First .. I =>
              Data_Array (J) = Data_Array'Loop_Entry (J) + Value);
      end loop;
      Last_Increment := Value;
   end Process_1;
end Foo;
