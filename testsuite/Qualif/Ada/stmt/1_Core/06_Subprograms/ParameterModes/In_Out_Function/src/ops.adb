with Support; use Support;

package body Ops is
   
   type T_Point is record
      X, Y : Integer;
   end record;
   
   function Set_Q (P : T_Point; Q: out T_Quarter) return Integer is
   begin
      if P.X > 0 and then P.Y > 0 then    -- # test-q1
         Q := Q1; -- # q1
         return 1; -- # q1
      elsif P.X > 0 and then P.Y < 0 then -- # test-q2
         Q := Q2; -- # q2
         return 2; -- # q2
      elsif P.X < 0 and then P.Y < 0 then -- # test-q3
         Q := Q3; -- # q3
         return 3; -- # q3
      elsif P.X < 0 and then P.Y > 0 then -- # test-q4
         Q := Q4; -- # q4
         return 4; -- # q4
      else
         Q := Q0; -- # q0
         return 0; -- # q0
      end if;
   end;
                       
   function Quarter (X, Y : Integer) return T_Quarter is      
      P : T_Point := (X => X, Y => Y);
      Q : T_Quarter;
      Hint : Integer;
   begin
      Hint := Set_Q (P, Q); -- # stmt
      Assert (Hint = T_Quarter'Pos (Q)); -- # stmt
      return Q; -- # stmt
   end;
   
end;
