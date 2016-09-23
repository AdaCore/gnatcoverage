package Ops is
   --                Y
   --                ^
   --         Q4     |    Q1    
   --     -----------+----------> X
   --         Q3     |    Q2
   --
   type T_Quarter is (Q0, Q1, Q2, Q3, Q4);
   
   function Quarter (X, Y : Integer) return T_Quarter;
end;
