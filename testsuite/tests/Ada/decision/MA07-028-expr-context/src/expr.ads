package Expr is

   GX : Integer := 0;
   pragma Volatile (GX);

   GB : Boolean;
   pragma Volatile (GB);

   procedure As_RHS       (A, B : Boolean);
   procedure As_ACTUAL    (A, B : Boolean);
   procedure As_CASE      (A, B : Boolean);
   procedure As_AGGREGATE (A, B : Boolean);
   procedure As_DECLINIT  (A, B : Boolean);
   procedure As_DISCINIT  (A, B : Boolean);

   function  As_RETURN (A, B : Boolean) return Boolean;

   procedure As_BODYPRECOND  (A, B : Boolean);
   procedure As_BODYPOSTCOND (A, B : Boolean);

   pragma Check_Policy (Precondition, On);
   pragma Check_Policy (Postcondition, On);

   procedure As_SPECPRECOND  (A, B : Boolean);
   pragma Precondition (A and then B); -- # freestanding-expr

   procedure As_SPECPOSTCOND (A, B : Boolean);
   pragma Postcondition (A and then B); -- # freestanding-expr

   procedure As_DEBUG_ASSERT (A, B : Boolean);
end;
