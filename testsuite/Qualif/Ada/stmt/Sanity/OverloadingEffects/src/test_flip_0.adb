with Support, Overloads; use Support, Overloads;

--  Dispatch to none of two subprograms called Flip. Verify that the statements
--  in both bodies are reported uncovered.

procedure Test_Flip_0 is
begin
   Dispatch (Flipx => False, Flipb => False);
end;

--# overloads.adb
--  /flipx/  l- s-
--  /flipb/  l- s-
