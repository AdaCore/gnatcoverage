--  Test driver for object declarations. It only "with"s the functional
--  subprograms ..._Swap that contain object declaration of interest, but does
--  not call any of them, so all the object declarations are expected to be
--  reported as uncovered.

with Access_Swap;
with Integer_Swap;
with Matrix_Swap;
with Private_Swap;
with Record_Derived_Swap;
with Record_Impl_Init_Swap;
with Record_Swap;

with Support; use Support;

procedure Test_Object_Declarations_No is
begin
   Assert (True);
end Test_Object_Declarations_No;

--# access_swap.adb
--  /stmt/ l- ## s-

--# integer_swap.adb
--  /stmt/ l- ## s-

--# matrix_swap.adb
--  /stmt/ l- ## s-

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

