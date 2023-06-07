with Monitor;
with Pack; use Pack;

procedure P is
begin
   I := I + 1;
   Q;
   R;
   if I > 2 then
      Monitor.Touch;
   end if;
end P;
