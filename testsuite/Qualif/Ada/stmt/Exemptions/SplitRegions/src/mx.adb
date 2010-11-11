with Multiple_Exemptions, Support; use Support;

package body MX is
   I, J, K : Integer;
   X1, X2, X3, Xh : Boolean;

   procedure Trigger_0XR is
   begin
      I := 1; J := 1; K := 1;
      Multiple_Exemptions (I, J, K, X1, X2, X3, Xh);
      Assert (not X1 and then not X2 and then not X3 and then not Xh);
   end;

   procedure Trigger_XR1 is
   begin
      I := 0; J := 0; K := 0;
      Multiple_Exemptions (I, J, K, X1, X2, X3, Xh);
      Assert (X1 and then not X2 and then not X3 and then Xh);
   end;

   procedure Trigger_XR2 is
   begin
      I := 3; J := -1; K := 1;
      Multiple_Exemptions (I, J, K, X1, X2, X3, Xh);
      Assert (I = 3 and then J = 2 and then K = 1);
      Assert (not X1 and then X2 and then not X3 and then not Xh);
   end;

   procedure Trigger_XR3 is
   begin
      I := 10; J := 1; K := -1;
      Multiple_Exemptions (I, J, K, X1, X2, X3, Xh);
      Assert (I = 10 and then J = 1 and then K = 9);
      Assert (not X1 and then not X2 and then X3 and then not Xh);
   end;
end;
