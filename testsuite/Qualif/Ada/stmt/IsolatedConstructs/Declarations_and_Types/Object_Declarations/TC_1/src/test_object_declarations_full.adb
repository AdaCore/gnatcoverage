--  Test driver for object declarations. Instatiates and calls everything from
--  the functional code, so object declaration is expected to be reported as
--  uncovered.

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

procedure Test_Object_Declarations_Full is
   package Decls_Pack_Matrix is new Decls_Pack_Matrix_G (1, 2);
   use Decls_Pack_Matrix;

   package Decls_Pack_Records is new Decls_Pack_Records_G;
   use Decls_Pack_Records;

   package Decls_Pack_Derived_Records is new Decls_Pack_Derived_Records_G;
   use Decls_Pack_Derived_Records;

   package Decls_Pack_Private is new Decls_Pack_Private_G;
   use Decls_Pack_Private;

   D_Var1 : Derived_Discrete_Coordinate := (10, 100);
   D_Var2 : Derived_Discrete_Coordinate := (20, 200);

   D_C_Var1 : Discrete_Coordinate := (10, 100);
   D_C_Var2 : Discrete_Coordinate := (20, 200);

   Coord1 : Coordinate := Coordinate_Zero;
   Coord2 : Coordinate := (1.0, 1.0);

   Coord_Var1 : Access_Coordinate := new Coordinate'(1.0, 2.0);
   Coord_Var2 : Access_Coordinate := new Coordinate'(3.0, 4.0);

   Int1 : Integer := 1;
   Int2 : Integer := 2;

   Matr1 : Matrix := (1 => (1 => 1));
   Matr2 : Matrix := (1 => (1 => 2));

   P_Var1 : T_Private := Get_Private (10);
   P_Var2 : T_Private := Get_Private (20);

   Var1 : Access_Integer := new Integer'(1);
   Var2 : Access_Integer := new Integer'(2);

   V1 : Derived_Coordinate := (1.0, 10.0);
   V2 : Derived_Coordinate := (2.0, 20.0);
begin
   Assert (Matrix_V = ((1, 2), (3, 4)));
   Assert (Derived_Discrete_Coordinate_V = (0, 0));
   Assert (My_String.Data = "Ada");
   Assert (Get_Integer (T_Private_C) = (0));

   --  Call all the ..._Swap procedures

   Access_Swap (Coord_Var1, Coord_Var2);
   Assert (Coord_Var1.X = 3.0 and then
           Coord_Var1.Y = 4.0 and then
           Coord_Var2.X = 1.0 and then
           Coord_Var2.Y = 2.0);

   Integer_Swap (Int1, Int2);
   Assert (Int1 = 2 and then Int2 = 1);

   Matrix_Swap (Matr1, Matr2);
   Assert (Matr1 (1, 1) = 2 and then Matr2 (1, 1) = 1);

   Private_Swap (P_Var1, P_Var2);
   Assert (Get_Integer (P_Var1) = 20 and then Get_Integer (P_Var2) = 10);

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

   --  Call all the subprograms from library packages
   Decls_Pack_1.Local_Swap (Int1, Int2);
   Assert (Int1 = 1 and then Int2 = 2);
   Assert (Decls_Pack_1.Local_Fun (Mon) = Tue);

   Decls_Pack_2.Local_Swap (Var1, Var2);
   Assert (Var1.all = 2 and then Var2.all = 1);
   Assert (Decls_Pack_2.Local_Fun (-1) = null);

   --  Call all subprograms from all instantiations

   Decls_Pack_Matrix.Local_Swap (Matr1, Matr2);
   Assert (Matr1 (1, 1) = 1 and then Matr2 (1, 1) = 2);

   Assert (Decls_Pack_Matrix.Local_Fun (Matr1) = (1 => (1 => -1)));

   Decls_Pack_Records.Local_Swap (Coord1, Coord2);
   Assert (Coord1 = Coordinate_Zero and then Coord2 = (1.0, 1.0));

   Assert (Decls_Pack_Records.Local_Fun (My_String).Data = "Beb");


   Decls_Pack_Derived_Records.Local_Swap (V1, V2);
   Assert (V1.X = 2.0  and then
           V1.Y = 20.0 and then
           V2.X = 1.0  and then
           V2.Y = 10.0);

   Assert (Decls_Pack_Derived_Records.Local_Fun (-1.0, -1.0) = (0.0, 0.0));

   Decls_Pack_Private.Local_Swap (P_Var1, P_Var2);
   Assert (Get_Integer (P_Var1) = 10 and then Get_Integer (P_Var2) = 20);

   Assert (Get_Integer (Decls_Pack_Private.Local_Fun (Get_Private (1))) = 100);
end Test_Object_Declarations_Full;

--# access_swap.adb
--  /stmt/ l+ 0

--# integer_swap.adb
--  /stmt/ l+ 0

--# matrix_swap.adb
--  /stmt/ l+ 0

--# private_swap.adb
--  /stmt/ l+ 0

--# record_swap.adb
--  /stmt/ l+ 0
--  /if/   l+ 0

--# record_derived_swap.adb
--  /stmt/ l+ 0
--  /if/   l+ 0

--# record_impl_init_swap.adb
--  /stmt/ l+ 0
--  /if/   l+ 0

--# decls_pack_1.ads
--  /dcls/    l+ 0
--  /g1_dcls/ l+ 0
--  /g2_dcls/ l+ 0

--# decls_pack_1.adb
--  /local_swap/    l+ 0
--  /decl/          l+ 0
--  /stmt/          l+ 0
--  /case1/         l+ 0
--  /case2/         l- s-

--  /g1_local_swap/ l+ 0
--  /g1_decl/       l+ 0
--  /g1_stmt/       l+ 0

--  /g2_local_swap/ l+ 0
--  /g2_decl/       l+ 0
--  /g2_stmt/       l+ 0

--# decls_pack_2.ads
--  /dcls/    l+ 0
--  /g1_dcls/ l+ 0
--  /g2_dcls/ l+ 0

--# decls_pack_2.adb
--  /local_swap/        l+ 0
--  /decl/              l+ 0
--  /stmt/              l+ 0
--  /in_if/             l+ 0

--  /g1_local_swap/      l+ 0
--  /g1_in_iflocal_swap/ l+ 0
--  /g1_decl/            l+ 0
--  /g1_stmt/            l+ 0
--  /g1_in_if/           l- s-

--  /g2_local_swap/      l+ 0
--  /g2_decl/            l+ 0
--  /g2_stmt/            l+ 0
--  /g2_case1/           l+ 0
--  /g2_case2/           l- s-
--  /g2_case3/           l- s-
