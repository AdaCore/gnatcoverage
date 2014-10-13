with Support, Mix; use Support;

procedure Test_1 is
begin
   Assert (Mix (A => True,
                B => True,  C => False, -- B or  C True            | = is T
                D => False, E => False, -- D and E False | /= is T | 
                Z => True,  T => True)  -- F or  G True  | 
             = True);
end;

--# mix.adb
--  /eval/ l! ## eF-:"A", eF-:"B", eT-:"D", eF-:"Z"
