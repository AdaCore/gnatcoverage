pragma Ada_2012;

package Pkg is

   function Forty_Two return Integer is (42);                       -- # decl

   --  Call as a record component's default value
   type Rec is record
      Number : Integer := Forty_Two;                                -- # comp
    end record;

   --  Call as a subprogram parameter's default value
   function Add (R : Rec; I : Integer := Forty_Two) return Integer  -- # spec
   is (R.Number + I);                                               -- # body

 end Pkg;
