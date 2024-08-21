with In_Proc_Decls;

--  Check the coverage of a generic procedure, generic null procedure, generic
--  function and a generic expression function declared in a regular procedure
--  when called. We expect the generic subprograms to be covered.  Currently,
--  the coverage of instanciations of generic subprograms is not computed.

procedure Test_In_Proc_Decls_Called is
begin
   In_Proc_Decls;
end Test_In_Proc_Decls_Called;

--# in_proc_decls.adb
-- /p/        l+ ## 0
-- /decl/     l+ ## 0
-- /stmt/     l+ ## 0
-- /fun/      l+ ## 0
-- /exp_fun/  l? ## s=>s?, f=>s?,f?
-- /null_fun/ l? ## s=>s?, f=>s?,f?
