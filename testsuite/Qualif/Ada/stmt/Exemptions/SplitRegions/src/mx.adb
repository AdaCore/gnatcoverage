with Multiple_Exemptions, Support; use Support;

package body MX is
   I, J, K : Integer;
   Xf : aliased Xflags;

   procedure Trigger_0XR is
   begin
      I := 1; J := 1; K := 1;
      Multiple_Exemptions (I, J, K, Xf'Access);
      Assert (not Xf.X1 and then not Xf.X2
                and then not Xf.X3 and then not Xf.Xh);
   end;

   procedure Trigger_XR1 is
   begin
      I := 0; J := 0; K := 0;
      Multiple_Exemptions (I, J, K, Xf'Access);
      Assert (Xf.X1 and then not Xf.X2
                and then not Xf.X3 and then Xf.Xh);
   end;

   procedure Trigger_XR2 is
   begin
      I := 3; J := -1; K := 1;
      Multiple_Exemptions (I, J, K, Xf'Access);
      Assert (not Xf.X1 and then Xf.X2
                and then not Xf.X3 and then not Xf.Xh);
   end;

   procedure Trigger_XR3 is
   begin
      I := 10; J := 1; K := -1;
      Multiple_Exemptions (I, J, K, Xf'Access);
      Assert (not Xf.X1 and then not Xf.X2
                and then Xf.X3 and then not Xf.Xh);
   end;
end;
