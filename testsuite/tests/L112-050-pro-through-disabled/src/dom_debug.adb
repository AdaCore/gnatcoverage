procedure Dom_Debug is

   procedure Proc is begin null; end Proc;
   function F return Integer is begin return 0; end F;

   X0 : Integer;
   pragma Debug (Proc);

   procedure P is begin null; end P;
   --  The presence of this body prevents direct chaining of the following
   --  declaration to X0, instead a dominance marker denoting the pragma Debug
   --  permits propagation.

   X1 : Integer := F;

begin
   null;
end Dom_Debug;
