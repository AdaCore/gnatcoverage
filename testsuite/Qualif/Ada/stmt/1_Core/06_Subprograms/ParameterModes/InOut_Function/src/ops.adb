pragma Ada_2012;

package body Ops is

   type T_Point is record
      X, Y : Integer;
      Q : T_Quarter;
   end record;

   function Set_Q (P : in out T_Point) return T_Quarter is
      Q : T_Quarter;
   begin
      if P.X > 0 and then P.Y > 0 then    -- # test-q1
         Q := Q1; -- # q1
      elsif P.X > 0 and then P.Y < 0 then -- # test-q2
         Q := Q2; -- # q2
      elsif P.X < 0 and then P.Y < 0 then -- # test-q3
         Q := Q3; -- # q3
      elsif P.X < 0 and then P.Y > 0 then -- # test-q4
         Q := Q4; -- # q4
      else
         Q := Q0; -- # q0
      end if;
      P.Q := Q; -- # stmt
      return Q; -- # stmt
   end;

   function Quarter (X, Y : Integer) return T_Quarter is
      P : T_Point := (X => X, Y => Y, Q => <>);
   begin
      return Set_Q (P); -- # stmt
   end;

end;
