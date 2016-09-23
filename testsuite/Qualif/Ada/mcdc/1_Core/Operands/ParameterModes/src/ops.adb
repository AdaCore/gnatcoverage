with Support; use Support;

package body Ops is
   
   type T_Status is tagged record
      A : T_Action;
      W : T_What_Ahead;
   end record;
   
   type T_Qualified_Status is new T_Status with record
      Ss : T_Safety_Status;
   end record;
   
   function Q1 (S : T_Status) return T_Safety_Status is
   begin
      if S.A = Step and then S.W = Pit then -- # test
         return Unsafe; -- # unsafe
      else
         return Safe; -- # safe
      end if;
   end;
   
   procedure Q2 (S : T_Status; Ss : out T_Safety_Status) is
   begin
      if S.A = Step and then S.W = Pit then -- # test
         Ss := Unsafe; -- # unsafe
      else
         Ss := Safe; -- # safe
      end if;
   end;
   
   procedure Q3 (S : in out T_Qualified_Status) is
   begin
      if S.A = Step and then S.W = Pit then -- # test
         S.Ss := Unsafe; -- # unsafe
      else
         S.Ss := Safe; -- # safe
      end if;
   end;
   
   function Q4 (S : in out T_Qualified_Status) return Integer is
   begin
      if S.A = Step and then S.W = Pit then -- # test
         S.Ss := Unsafe; -- # unsafe
      else
         S.Ss := Safe; -- # safe
      end if;
      return 0;
   end;
   
   function Qualify (A : T_Action; W : T_What_Ahead) return T_Safety_Status is
      S1 : T_Safety_Status;
   begin
      S1 := Q1 ((A, W));
      
      declare
         S2 : T_Safety_Status;
      begin
         Q2 ((A, W), S2);
         Assert (S2 = S1);
      end;
      
      declare
         QS : T_Qualified_Status := (A => A, W => W, SS => <>);
      begin
         Q3 (QS);
         Assert (QS.Ss = S1);
      end;
      
      declare
         QS : T_Qualified_Status := (A => A, W => W, SS => <>);
         X : Integer;
      begin
         X := Q4 (QS);
         Assert (QS.Ss = S1);
      end;

      return S1;
   end;
   
end;
