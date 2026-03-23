with P;

procedure Test_P is
   Prio : P.Priority := P.Prio_Val;
   pragma Volatile (Prio);
begin
   if Prio /= P.Priority'First then
      raise Program_Error;
   end if;
end;

--  The "complex" decision case, even though involving a
--  static expression as well, is provided through an expression
--  function for which the compiler generates code.
--
--  The simpler "eval" case boils down to a static constant,
--  for which no code at all is visible to binary traces.

--# p.ads
--
-- /complex/ l+ ## 0
--
--%opts: --trace-mode=bin
-- /eval/ l. ## 0
--
--%opts: --trace-mode=src
-- /eval/ l+ ## 0
