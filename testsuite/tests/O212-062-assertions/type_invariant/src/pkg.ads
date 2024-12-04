pragma Ada_2012;
pragma Assertion_Policy (Check);

package Pkg is

   type Rec is private;                                      -- # rec_decl

   function Make_Rec (B : Boolean) return Rec;

private

   type Rec is record                                        -- # rec_def
      B : Boolean := True;                                   -- # rec_b
   end record                                                -- # rec_end
   with Type_Invariant => B;                                 -- # type_inv
   --  Record with a type invariant expressed as an aspect

   function Make_Rec (B : Boolean) return Rec is ((B => B)); -- # return

end Pkg;
