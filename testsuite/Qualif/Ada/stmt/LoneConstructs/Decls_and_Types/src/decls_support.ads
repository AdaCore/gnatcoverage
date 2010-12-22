--  This package contains various declarations needed for tests aimed at
--  checking features from Chapter 3 "Declarations and Types"
with Support; use Support;
package Decls_Support is

   --  Scalar types:
   type Week_Day is (Mon, Tue, Wen, Thu, Fri, Sat, Sun);

   subtype Index is Natural range 0 .. 100;

   --  Record types
   type Var_String (Len : Index := 0) is record
      Data : String (1 .. Len);
   end record;

   type Coordinate is record
      X, Y : Float := 0.0;
   end record;

   Coordinate_Zero : constant Coordinate := (X => 0.0, Y => 0.0);

   type Discrete_Coordinate is record
      X : Integer := Identity (0);
      Y : Integer := Identity (0);
   end record;

   --  Array types:
   type Vector is array (Integer range <>) of Integer;

   type Matrix is array (Integer range <>, Integer range <>) of Integer;
   subtype Small_Matrix is Matrix (1 .. 2, 1 .. 2);

   -- Private types:
   type T_Private is private;
   T_Private_Zero : constant T_Private;

   function Get_Private (I : Integer) return T_Private;
   function Get_Integer (X : T_Private) return Integer;
   --  Sets and gets the value of the integer component of the full type
   --  declaration

   --  Access types
   type Access_Integer is access Integer;
   type Access_All_Integer is access all Integer;
   type Access_Const_Integer is access constant Integer;
   type Access_Coordinate is access Coordinate;

   --  Derived types

   type Derived_Discrete_Coordinate is new Discrete_Coordinate;
   type Derived_Coordinate is new Coordinate;

   --  Miscellaneous

   Integer_Var : aliased Integer := 1;

private
   type T_Private is record
      I : Integer := 0;
   end record;

   T_Private_Zero : constant T_Private := (I => 0);

end Decls_Support;
