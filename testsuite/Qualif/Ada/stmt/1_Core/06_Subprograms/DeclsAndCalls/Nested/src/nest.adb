with Support; use Support;

package body Nest is
   
   Value : Integer := 0;
   pragma Volatile (Value);
   
   procedure Touch (Cd : access Cdata) is
   begin
      Cd.Nops := Cd.Nops + 1; -- # touch
   end;
   
   procedure Check 
     (Lfun, Lproc, Pfun, Pproc, Indirect : Boolean; Cd : access Cdata)
   is
      -- nested procedure and function
      
      procedure Inc (X : in out Integer; Cd : access Cdata) is
      begin
	 Touch (Cd); -- # lproc
	 X := X + 1; -- # lproc
      end;
      
      function Positive (X : Integer; Cd : access Cdata) return Boolean is
      begin
	 Touch (Cd);   -- # lfun
	 return X > 0; -- # lfun
      end;
      
      -- nested package
      
      package Ops is
	 
	 --  visible operations
	 
	 procedure Dec (X : in out Integer; Cd : access Cdata);
	 function Negative (X : Integer; Cd : access Cdata) return Boolean;
	 
      private
	 -- private operations

	 function Neg_P (X : Integer) return Boolean;
      end;
      
      package body Ops is
	 
	 -- internal subpgram (package body only)
	 
	 procedure My_Dec (X : in out Integer) is
	 begin
	    X := X - 1; -- # pproc
	 end;
	 
	 procedure Dec (X : in out Integer; Cd : access Cdata) is
	 begin
	    Touch (Cd);     -- # pproc
	    My_Dec (Value); -- # pproc
	 end;
      
	 function Neg_P (X : Integer) return Boolean is
	 begin
	    return X < 0; -- # pfun
	 end;
	 
	 function Negative (X : Integer; Cd : access Cdata) return Boolean is
	 begin
	    Touch (Cd);       -- # pfun
	    return Neg_P (X); -- # pfun
	 end;
      end;
      
      type Opcodes is mod 4;
      Op_Lfun : constant := 1;
      Op_Pfun : constant := 2;
      
      type Trigger (Opc : Opcodes; Value : Integer; Cd : access Cdata) is
	 record
	    case Opc is
	       when 0 => null;
	       when 1 =>
		  Pos : Boolean := Positive (Value, Cd);
	       when 2 =>
		  Neg : Boolean := Ops.Negative (Value, Cd);
	       when 3 =>
		  P : Boolean := Positive (Value, Cd);
		  N : Boolean := Ops.Negative (Value, Cd);	       
	    end case;
	 end record;
      
      Opc : Opcodes := 0;
   begin
      Value := 5; -- # check
      
      if Lfun then -- # check
	 if Indirect then -- # lfun
	    Opc := Opc or Op_Lfun; -- # lfi
	 else
	    Assert (Positive (Value, Cd)); -- # lfd
	 end if;
      end if;
      
      if Lproc then -- # check
	 Inc (Value, Cd); -- # lpd
      end if;
      
      if Pfun then -- # check
	 if Indirect then  -- # pfun
	    Opc := Opc or Op_Pfun; -- # pfi
	 else
	    Assert (not Ops.Negative (Value, Cd)); -- # pfd
	 end if;
      end if;
      
      if Pproc then -- # check
	 Ops.Dec (Value, Cd); -- # ppd
      end if;
      
      if Indirect then -- # check
	 declare
	    T : Trigger (Opc => Opc, Value => Value, Cd => Cd); -- # indirect
	 begin
	    null; -- # indirect
	 end;
      end if;
      
   end;
		    
end;
