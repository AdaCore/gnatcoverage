pragma Ada_2012;
package body Ops is
   
   -- Decision on the if-expression below. Single operand and
   -- no statement involved, so no dominance info available.
   
   function "abs" (X : Int64) return Uns64 is
      (if X = Int64'First  -- # eval
         then 2**63
         else Uns64 (Int64'(abs X)));

  --
     
   procedure Process (X, Y : Int64) is
        
      -- Below, two calls to the "abs" expression function
      -- above in a row. When both calls are inlined, special
      -- treatment is required to untangle the two decisions.

      Xu : Uns64 := abs X with Volatile;
      Yu : Uns64 := abs Y with Volatile;
   begin
      Rx := Xu;
      Ry := Yu;
   end;

end;


