pragma Ada_2022;

package Gen is

   generic
      type Value_Type is private;
      with function Weight (Self : Value_Type) return Natural;
   function Weight_Sum (Left, Right : Value_Type) return Natural;

   generic
      type Value_Type is private;
      with function Weight (Self : Value_Type) return Natural;
   function Both_Weights_Null (Left, Right : Value_Type) return Boolean;

   function Weight_Sum (Left, Right : Value_Type) return Natural is
     (Weight (Left) + Weight (Right)); -- # expr_st

   function Both_Weights_Null (Left, Right : Value_Type) return Boolean is
     (Weight (Left) = 0 and then Weight (Right) = 0);  -- # expr_dc :o/e:

end Gen;
