pragma Ada_2012;

with Pkg; use Pkg;
with Pkg2;
with Pkg3;
with Pkg3.Child;
with Pkg4;
with Pkg6;
with Pkg7.Child;
--  with Pkg7;

procedure Make_Call                                            -- # test
is
   type Pt_Acc is access function (X, Y : Float) return Point; -- # decl

   function Run_Callback (Callback : Pt_Acc) return Point is   -- # fun
   begin
      return Callback (1.0, 2.00);                             -- # call_0
   end;

   Set_Acc : constant Pt_Acc := Set'Access;                    -- # decl

   Dummy : Float := Run_Callback (Set_Acc).X;                  -- # call_1

   --  Return type Type_A declared in unimported Pkg1
   Dummy_A : Boolean := Pkg2.Foo.Field;                        -- # call_a

   --  Return type Type_B declared in unimported Pkg2's child package
   Dummy_B : Boolean := Pkg2.Bar.Field;                        -- # call_b

   --  Return type Type_C declared in imported Pkg3
   Dummy_C : Boolean := Pkg4.Foo.Field;                        -- # call_c

   --  Return type Type_D declared in imported Pkg3.Child
   Dummy_D : Boolean := Pkg4.Bar.Field;                        -- # call_d

   --  Pkg6.Bar calls Pkg6.Foo, defined in the private part of Pkg6. Pkg6.Foo
   --  performs a call to Pkg5.Id, which returns type Pkg5.T that it only
   --  private-withed in Pkg6. This checks that instrumenting a call returning
   --  a private-withed type runs smoothly.
   Dummy_E : Boolean := Pkg6.Bar;                              -- # call_e

   --  Return Type_F, which is defined in unimported Pkg6, parent of Pkg.Child
   --  where Foo is defined.
   Dummy_F : Boolean := Pkg7.Child.Foo.Field;                  -- # call_f
begin
   null;                                                       -- # null
end Make_Call;
