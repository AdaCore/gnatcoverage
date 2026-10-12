pragma Ada_2022;
pragma Assertion_Policy (Check);

--  Contracts expressed as pragmas instead of aspects. Such pragmas must
--  immediately follow the declaration they apply to, so no witness statement
--  can be inserted for them: they are handled as free-standing decisions.

package Pkg is

   function Foo (A, B : Boolean) return Boolean;
   pragma Precondition (A or else B);                  -- # foo_pre
   pragma Postcondition (Foo'Result or else not A);    -- # foo_post

   function Bar (A, B : Boolean) return Boolean;

   type Rec is private;                                -- # rec_decl

   function Make_Rec (B : Boolean) return Rec;

private

   type Rec is record                                  -- # rec_def
      B : Boolean := True;                             -- # rec_b
   end record;                                         -- # rec_end
   pragma Type_Invariant (Rec, Rec.B);                 -- # type_inv

   function Make_Rec (B : Boolean) return Rec is ((B => B)); -- # make_rec

end Pkg;
