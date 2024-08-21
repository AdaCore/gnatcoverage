with In_Proc_Decls;

--  Check the coverage of a generic procedure, generic null, procedure, generic
--  function and a generic expression function declared in a regular procedure
--  when not called. We expect the generic subprograms to not be covered.
--  Currently, the coverage of instanciations of generic subprograms is not
--  computed.

procedure Test_In_Proc_Decls_Not_Called is
begin
   null;
end Test_In_Proc_Decls_Not_Called;

--# in_proc_decls.adb
-- /p/        l- ## f-
-- /decl/     l- ## s-
-- /stmt/     l- ## s-
-- /fun/      l- ## f-
-- /exp_fun/  l? ## s=>s?, f=>s?,f?
-- /null_fun/ l? ## s=>s?, f=>s?,f?
-- /cstmt/    l- ## s-,c-
