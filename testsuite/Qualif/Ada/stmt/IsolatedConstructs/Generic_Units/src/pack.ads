package Pack is
   generic
      type Int is range <>;
   function New_Value_G (I : Int) return Int;

   generic
      type T is private;
      Init_Value : T;
   package Pack_G is
      Var : T := Init_Value;
      procedure Swap (A, B : in out T);
   end Pack_G;

   procedure Proc (I, J : in out Integer);

end Pack;
