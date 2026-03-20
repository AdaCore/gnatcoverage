pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   function Fact (I : Integer) return Integer is
     (if I < 2
      then 1
      else I * Fact (I - 1));

    X, Y : Boolean;
    function X_And_Then_Y return Boolean is (X and then Y);

begin
   Put_Line ("Fact (6) = " & Integer'Image (Fact (6)));
end Main;
