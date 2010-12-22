package body Decls_Support is

   function Get_Private (I : Integer) return T_Private is
   begin
      return (I => I);
   end Get_Private;

   function Get_Integer (X : T_Private) return Integer is
   begin
      return X.I;
   end Get_Integer;

end Decls_Support;
