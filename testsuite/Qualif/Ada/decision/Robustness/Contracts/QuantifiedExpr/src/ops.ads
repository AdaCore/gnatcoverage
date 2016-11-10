pragma Ada_2012;

package Ops is
   pragma Assertion_Policy (Ignore);
   
   Data : array (1 .. 50) of Integer with Volatile;
   
   procedure Add_Or_Mult (X, Y : Integer; Z : out Integer) with
     Pre  => (for some X of Data => X < 0 or else X mod 2 /= 0),
     Post => (for all X of Data => X > 0 and then X mod 2 = 0);

end;
