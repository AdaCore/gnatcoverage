--  Check that the coverage report mentions only postconditions in
--  Contract_Cases.

with Contract_Cases;

procedure Test_Contract_Cases is
begin
   if Contract_Cases.F (5) /= 2 then
      raise Program_Error;
   end if;
end Test_Contract_Cases;

--# contract_cases.ads
-- /conseq-1/ s=>l., ac=>l+ ## 0
-- /conseq-2/ s=>l., ac=>l- ## a-
--
--%cov: --level=stmt --postcond-only
-- /guard-1/  l. ## 0
--%cov: --level=stmt\+atc --postcond-only
-- /guard-1/  l. ## 0
--%cov: --level=stmt\+atcc --postcond-only
-- /guard-1/  l. ## 0
--
--%cov: --level=stmt\+decision\+atc --postcond-only
-- /guard-1/  l! ## dF-
--%cov: --level=stmt\+decision\+atcc --postcond-only
-- /guard-1/  l! ## dF-
--
--%cov: --level=stmt\+(uc_)?mcdc\+atcc --postcond-only
-- /guard-1/  l! ## dF-
