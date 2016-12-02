pragma Unsuppress (All_Checks);
pragma Check_Float_Overflow;

package body Ops is
   
   function One_Ok (A, B : Capsum) return Boolean is
   begin
      if A.X + A. Y <= A.Cap or else B.X + B.Y <= B.Cap then -- # test
	 N_Ok := N_Ok + 1; -- # ok
	 return True;      -- # ok
      else
	 N_Ko := N_Ko + 1; -- # ko
	 return False;     -- # ko
      end if;
   exception
      when Constraint_Error =>
	 N_Ov := N_Ov + 1; -- # ov
	 return False; -- # ov
   end;
end;
