package Pkg is
   type Test_Type;
   type Test_Type_Ptr is access all Test_Type;
   type Test_Type is record
       Test_Var : Boolean;
   end record;
end Pkg;
