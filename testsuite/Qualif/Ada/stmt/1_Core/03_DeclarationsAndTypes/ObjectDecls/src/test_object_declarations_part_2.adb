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

procedure Test_Object_Declarations_Part_2 is
   package Decls_Pack_Records is new Decls_Pack_Records_G;
   use Decls_Pack_Records;

   package Decls_Pack_Private is new Decls_Pack_Private_G;
   use Decls_Pack_Private;

   Coord1 : Coordinate := Coordinate_Zero;
   Coord2 : Coordinate := (1.0, 1.0);

   P_Var1 : T_Private := Get_Private (10);
   P_Var2 : T_Private := Get_Private (20);

   D_Var1 : Derived_Discrete_Coordinate := (10, 100);
   D_Var2 : Derived_Discrete_Coordinate := (20, 200);

   D_C_Var1 : Discrete_Coordinate := (10, 100);
   D_C_Var2 : Discrete_Coordinate := (20, 200);

   Coord_Var1 : Access_Coordinate := new Coordinate'(1.0, 2.0);
   Coord_Var2 : Access_Coordinate := new Coordinate'(3.0, 4.0);

   Int1 : Integer := 1;
   Int2 : Integer := 2;

   Matr1 : Matrix := (1 => (1 => 1));
   Matr2 : Matrix := (1 => (1 => 2));

   Var1 : Access_Integer := new Integer'(1);
   Var2 : Access_Integer := new Integer'(2);

   V1 : Derived_Coordinate := (1.0, 10.0);
   V2 : Derived_Coordinate := (2.0, 20.0);
begin
   Assert (My_String.Data = "Ada");
   Assert (Get_Integer (T_Private_C) = (0));

   --  Call half of ..._Swap procedures
   Private_Swap (P_Var1, P_Var2);
   Assert (Get_Integer (P_Var1)  = 20 and then Get_Integer (P_Var2)  = 10);

   Record_Derived_Swap (D_Var1, D_Var2);
   Assert (D_Var1.X = 20  and then
           D_Var1.Y = 200 and then
           D_Var2.X = 10  and then
           D_Var2.Y = 100);

   Record_Impl_Init_Swap (D_C_Var1, D_C_Var2);
   Assert (D_C_Var1.X = 20  and then
           D_C_Var1.Y = 200 and then
           D_C_Var2.X = 10  and then
           D_C_Var2.Y = 100);

   Record_Swap (Coord1, Coord2);
   Assert (Coord2 = Coordinate_Zero and then Coord1 = (1.0, 1.0));

   --  Call subprograms from library packages
   Assert (Decls_Pack_1.Local_Fun (Mon) = Tue);

   Assert (Decls_Pack_2.Local_Fun (-1) = null);

   --  Call subprograms from instantiations
   Assert (Decls_Pack_Records.Local_Fun (My_String).Data = "Beb");
   Assert (Get_Integer (Decls_Pack_Private.Local_Fun (Get_Private (1))) = 100);
end Test_Object_Declarations_Part_2;

--# access_swap.adb
--  /stmt/ l- ## s-

--# integer_swap.adb
--  /stmt/ l- ## s-

--# matrix_swap.adb
--  /stmt/ l- ## s-

--# private_swap.adb
--  /stmt/ l+ ## 0

--# record_swap.adb
--  /stmt/ l+ ## 0
--  /if/   l+ ## 0

--# record_derived_swap.adb
--  /stmt/ l+ ## 0
--  /if/   l+ ## 0

--# record_impl_init_swap.adb
--  /stmt/ l+ ## 0
--  /if/   l+ ## 0

--# decls_pack_1.ads
--  /dcls/    l+ ## 0
--  /g1_dcls/ ~l- ## ~s-
--  /g2_dcls/ l+ ## 0

--# decls_pack_1.adb
--  /local_swap/    l- ## s-
--  /decl/          l+ ## 0
--  /stmt/          l+ ## 0
--  /case1/         l+ ## 0
--  /case2/         l- ## s-

--  /g1_local_swap/ ~l- ## ~s-
--  /g1_decl/       ~l- ## ~s-
--  /g1_stmt/       ~l- ## ~s-

--  /g2_local_swap/ ~l- ## ~s-
--  /g2_decl/       l+ ## 0
--  /g2_stmt/       l+ ## 0

--# decls_pack_2.ads
--  /dcls/    l+ ## 0
--  /g1_dcls/ ~l- ## ~s-
--  /g2_dcls/ ~l+ ## 0

--# decls_pack_2.adb
--  /local_swap/        l- ## s-
--  /decl/              l+ ## 0
--  /stmt/              l+ ## 0
--  /in_if/             l+ ## 0

--  /g1_local_swap/      ~l- ## ~s-
--  /g1_in_iflocal_swap/ ~l- ## ~s-
--  /g1_decl/            ~l- ## ~s-
--  /g1_stmt/            ~l- ## ~s-
--  /g1_in_if_stmt/      ~l- ## ~s-

--  /g2_local_swap/      ~l- ## ~s-
--  /g2_decl/            l+ ## 0
--  /g2_stmt/            l+ ## 0
--  /g2_case1/           l+ ## 0
--  /g2_case2/           l- ## s-
--  /g2_case3/           l- ## s-
