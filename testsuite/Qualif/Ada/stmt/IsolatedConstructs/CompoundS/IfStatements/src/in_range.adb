function In_Range (X , Min, Max : Integer) return Boolean is
begin
   if X < Min then     -- # XcmpMin
      return False;    -- # XoutMin
   elsif X > Max then  -- # XcmpMax
      return False;    -- # XoutMax
   else
      return True;     -- # Xin
   end if;
end;
