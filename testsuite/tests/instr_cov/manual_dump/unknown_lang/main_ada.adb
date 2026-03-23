with Interfaces.C; use Interfaces.C;

with Support;

procedure Main_Ada is
   function ASM_Ident (X : int) return int;
   pragma Import (C, ASM_Ident, "ident");
begin
   Support.Assert (ASM_Ident (3) = 3);
   pragma Annotate (Xcov, Dump_Buffers);
end Main_Ada;
