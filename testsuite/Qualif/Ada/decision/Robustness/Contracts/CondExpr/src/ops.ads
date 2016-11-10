pragma Ada_2012;

package Ops is
   pragma Assertion_Policy (Ignore);
   
   procedure Add_Or_Mult (X, Y : Integer; Z : out Integer) with
     Pre  => (if X > 0 then Y > 0 else Y <= 0),
     Post => (case X is
                when 1 .. 3 => Z = X + Y,
                when others => Z = X * Y);
end;
