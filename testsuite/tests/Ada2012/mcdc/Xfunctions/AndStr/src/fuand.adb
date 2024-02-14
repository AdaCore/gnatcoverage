with Support; use Support;

package body FUAND is

   function To_String (C : Coord) return String is
   begin
      return (1 .. 1 => Character'Val (48 - C));
   end;

   function Line (O : Object) return String is
   begin
      return To_String (O.X);
   end;

   function Col (O : Object) return String is
   begin
      return To_String (O.Y);
   end;

   --

   type Red_Circle is new Object with null record;

   function Shape (O : Red_Circle) return String;
   function Color (O : Red_Circle) return String;

   function Shape (O : Red_Circle) return String is
   begin
      return "circle";
   end;

   function Color (O : Red_Circle) return String is
   begin
      return "red";
   end;

   --

   type Blue_Square is new Object with null record;

   function Shape (O : Blue_Square) return String;
   function Color (O : Blue_Square) return String;

   function Shape (O : Blue_Square) return String is
   begin
      return "square";
   end;

   function Color (O : Blue_Square) return String is
   begin
      return "blue";
   end;

   --

   procedure TEST_A is
      A : Red_Circle := (X => 1, Y => 2);
      B : Blue_Square := (X => 1, Y => 2);
   begin
      Assert (Fight (A, B));      -- T T
      Assert (not Fight (A, A));  -- F T
   end;

   procedure TEST_B is
      A : Red_Circle := (X => 1, Y => 2);
      B : Blue_Square := (X => 1, Y => 2);
      C : Blue_Square := (X => 1, Y => 3);
   begin
      Assert (Fight (A, B));     -- T T
      Assert (not Fight (A, C)); -- T F
   end;

   procedure TEST_T is
      A : Red_Circle := (X => 1, Y => 2);
      B : Blue_Square := (X => 1, Y => 2);
   begin
      Assert (Fight (A, B));
   end;

end;
