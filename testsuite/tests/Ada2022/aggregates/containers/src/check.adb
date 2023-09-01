pragma Ada_2022;

with Ada.Containers; use Ada.Containers;

with Pkg; use Pkg;

with Support; use Support;

procedure Check (Input : Int_Set) is
   use Int_Maps;
   Res : constant Int_Map := Overly_Complex_Identity_Build (Input);
begin
   Assert (Input.Length = Res.Length);
   Assert ((for all Val of Input =>
               (declare Cur : constant Cursor := Res.Find (Val);
               begin Has_Element (Cur)
                     and then Key (Cur) = Val
                     and then Element (Cur) = Val)));
end Check;
