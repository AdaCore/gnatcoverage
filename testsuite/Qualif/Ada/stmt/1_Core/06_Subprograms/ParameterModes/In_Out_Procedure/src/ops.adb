package body Ops is
   
   type T_Point is record
      X, Y : Integer;
   end record;
   
   procedure Set_Q (P : T_Point; Q : out T_Quarter) is
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
   end;
                       
   function Quarter (X, Y : Integer) return T_Quarter is      
      Q : T_Quarter;
   begin
      Set_Q ((X => X, Y => Y), Q); -- # stmt
      return Q; -- # stmt
   end;
   
end;
