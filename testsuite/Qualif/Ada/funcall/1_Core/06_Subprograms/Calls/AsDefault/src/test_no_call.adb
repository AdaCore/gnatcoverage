with Pkg; use Pkg;

--  Test the coverage of call made to set default values.
--  For calls as default values of record components, we expect coverage to be
--  computed and reported as for any other call.
--
--  LIMITATION
--  For calls as default value of function parameters, we expect no SCO to be
--  associated with the call and for the call not to be instrumented. There
--  should be no call coverage violation reported if the call is never executed.

procedure Test_No_Call is
begin
    null;
end Test_No_Call;

--# pkg.ads
-- /decl/ l- ## s-,f-
-- /comp/ l! ## c-
-- /spec/ l- ## f-
-- /body/ l- ## s-
