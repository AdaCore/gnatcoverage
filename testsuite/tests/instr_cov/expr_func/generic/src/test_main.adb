pragma Ada_2012;

with Pkg;

procedure Test_Main is

   type Rec_Type is record
      Value : Natural;
   end record;

   function Weight (Self : Rec_Type) return Natural is (Self.Value);

   function Weight_Sum is new Pkg.Weight_Sum (Rec_Type, Weight);
   function Both_Weights_Null is new Pkg.Both_Weights_Null (Rec_Type, Weight);

   A : constant Rec_Type := (Value => 1);
   B : constant Rec_Type := (Value => 2);

begin
   if Weight_Sum (A, B) /= 3 then
      raise Program_Error;
   end if;
   if Both_Weights_Null (A, B) then
      raise Program_Error;
   end if;
end Test_Main;
