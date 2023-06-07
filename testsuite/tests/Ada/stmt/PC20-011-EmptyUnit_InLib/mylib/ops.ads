-- Unit with only dummy statements, not generating code.  Make sure
-- these aren't ignored by gnatcov and would get a violation if the
-- unit is perceived by gnatcov as absent from the test-closure.
--
--  pragma Pure; would be a bad choice, for example.

package Ops is
   X : Integer; -- # decl
end;
