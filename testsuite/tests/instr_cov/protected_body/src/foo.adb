pragma Ada_2012;

package body Foo is

   protected body Obj_Type is

      procedure Set (X : Integer) is
      begin
         Val := X;
      end Set;

      procedure Do_Nothing (X : Integer) is null;  -- # null_proc

      function Get return Integer is (Val);  -- # int_expr

      function Cond return Boolean is (Val > 0 or else Val = -5);  -- # bool_expr

   end Obj_Type;

end Foo;
