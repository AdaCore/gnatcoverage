--  Test driver for object declarations. It "with"s all the functional code,
--  including library packages, so library-level object declarations are
--  expected to be reported as covered. It also calls some subprograms from the
--  functional code, instantiates some generics and calls some routines from
--  the instantuations, so some local object declarations and object
--  declarations from generics are expected to be reported as covered, and some
--  other - as non-covered.
--
--  This test driver makes about a half of the object declarations of interest
--  elaborated. Test_Object_Declarations_Part_1 does this for another half.

with Access_Swap;
with Integer_Swap;
with Matrix_Swap;
with Private_Swap;
with Record_Derived_Swap;
with Record_Impl_Init_Swap;
with Record_Swap;

with Decls_Pack_1;  use Decls_Pack_1;
with Decls_Pack_2;  use Decls_Pack_2;

with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Object_Declarations_Part_1 is

   package Decls_Pack_Matrix is new Decls_Pack_Matrix_G (1, 2);
   use Decls_Pack_Matrix;
   
   package Decls_Pack_Derived_Records is new Decls_Pack_Derived_Records_G;
   use Decls_Pack_Derived_Records;

   Coord_Value1 : aliased Coordinate := (1.0, 2.0);
   Coord_Value2 : aliased Coordinate := (3.0, 4.0);
   Coord_Var1 : Access_All_Coordinate := Coord_Value1'Unchecked_Access;
   Coord_Var2 : Access_All_Coordinate := Coord_Value2'Unchecked_Access;

   Int1 : aliased Integer := 1;
   Int2 : aliased Integer := 2;

   Var1 : Access_All_Integer := Int1'Unchecked_Access;
   Var2 : Access_All_Integer := Int2'Unchecked_Access;

   Matr1 : Matrix := (1 => (1 => 1));
   Matr2 : Matrix := (1 => (1 => 2));
   
   V1 : Derived_Coordinate := (1.0, 10.0);
   V2 : Derived_Coordinate := (2.0, 20.0);
begin
   Assert (Matrix_V = ((1, 2), (3, 4)));
   Assert (Derived_Discrete_Coordinate_V = (0, 0));

   --  Call half of ..._Swap procedures

   Access_Swap (Coord_Var1, Coord_Var2);
   Assert (Coord_Var1.X = 3.0 and then
           Coord_Var1.Y = 4.0 and then
           Coord_Var2.X = 1.0 and then
           Coord_Var2.Y = 2.0);

   Integer_Swap (Int1, Int2);
   Assert (Int1 = 2 and then Int2 = 1);

   Matrix_Swap (Matr1, Matr2);
   Assert (Matr1 (1, 1) = 2 and then Matr2 (1, 1) = 1);
   
   --  Call subprograms from library packages
   Decls_Pack_1.Local_Swap (Int1, Int2);
   Assert (Int1 = 1 and then Int2 = 2);

   Decls_Pack_2.Local_Swap (Var1, Var2);
   Assert (Var1.all = 2 and then Var2.all = 1);

   --  Call subprograms from instantiations

   Decls_Pack_Matrix.Local_Swap (Matr1, Matr2);
   Assert (Matr1 (1, 1) = 1 and then Matr2 (1, 1) = 2);

   Decls_Pack_Derived_Records.Local_Swap (V1, V2);
   Assert (V1.X = 2.0  and then
           V1.Y = 20.0 and then
           V2.X = 1.0  and then
           V2.Y = 10.0);

end Test_Object_Declarations_Part_1;

--# access_swap.adb
--  /stmt/ l+ ## 0

--# integer_swap.adb
--  /stmt/ l+ ## 0

--# matrix_swap.adb
--  /stmt/ l+ ## 0

--# private_swap.adb
--  /stmt/ l- ## s-

--# record_swap.adb
--  /stmt/ l- ## s-

--# record_derived_swap.adb
--  /stmt/ l- ## s-
--  /if/   l- ## s-

--# record_impl_init_swap.adb
--  /stmt/ l- ## s-
--  /if/   l- ## s-

--# decls_pack_1.ads
--  /dcls/    l+ ## 0
--  /g1_dcls/ l+ ## 0
--  /g2_dcls/ ~l- ## ~s-

--# decls_pack_1.adb
--  /local_swap/    l+ ## 0
--  /decl/          l- ## s-
--  /stmt/          l- ## s-
--  /case1/         l- ## s-
--  /case2/         l- ## s-

--  /g1_local_swap/ l+ ## 0
--  /g1_decl/       ~l- ## ~s-
--  /g1_stmt/       ~l- ## ~s-

--  /g2_local_swap/ ~l- ## ~s-
--  /g2_decl/       ~l- ## ~s-
--  /g2_stmt/       ~l- ## ~s-

--# decls_pack_2.ads
--  /dcls/    l+ ## 0
--  /g1_dcls/ l+ ## 0
--  /g2_dcls/ ~l- ## ~s-

--# decls_pack_2.adb
--  /local_swap/        l+ ## 0
--  /decl/              l- ## s-
--  /stmt/              l- ## s-
--  /in_if/             l- ## s-

--  /g1_local_swap/      l+ ## 0
--  /g1_in_iflocal_swap/ l+ ## 0
--  /g1_decl/            ~l- ## ~s-
--  /g1_stmt/            ~l- ## ~s-
--  /g1_in_if_stmt/      ~l- ## ~s-

--  /g2_local_swap/      ~l- ## ~s-
--  /g2_decl/            ~l- ## ~s-
--  /g2_stmt/            ~l- ## ~s-
--  /g2_case1/           ~l- ## ~s-
--  /g2_case2/           ~l- ## ~s-
--  /g2_case3/           ~l- ## ~s-
