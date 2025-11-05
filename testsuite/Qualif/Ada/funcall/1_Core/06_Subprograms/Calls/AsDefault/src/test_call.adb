with Pkg; use Pkg;

--  Test the coverage of call made to set default values.
--  For calls as default values of record components, we expect coverage to be
--  computed and reported as for any other call.
--
--  LIMITATION
--  For calls as default value of function parameters, we expect no SCO to be
--  associated with the call and for the call not to be instrumented. There
--  should be no call coverage indication reported if the call is executed.

procedure Test_Call
is
    R : Rec;
    Dummy : Integer := Add (R);
begin
    null;
end Test_Call;

--# pkg.ads
-- /decl/ l+ ## 0
-- /comp/ l+ ## 0
-- /spec/ l+ ## 0
-- /body/ l+ ## 0
