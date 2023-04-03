package body Ops is

   -- req-rule #4

   pragma Unsuppress (All_Checks);
   I2 : Integer := 2;
   pragma Volatile (I2);

   procedure Fail_Range_Check is
      subtype R1_1 is Integer range 1 .. 1;
      X : R1_1; -- # stmt
   begin
      X := I2; -- # stmt
   end;

   procedure Div_CE (X, Y : Integer; T : out Integer) is
   begin
      if Y = 0 then -- # stmt
         raise Constraint_Error; -- # div_fault req-rule #1 and #5
      end if;
      T := X / Y; -- # no_div_fault
   end;

   Div_Error : exception;

   procedure Div_DE (X, Y : Integer; T : out Integer) is
   begin
      if Y = 0 then -- # stmt
         raise Div_Error; -- # div_fault req-rule #1 and #5
      end if;
      T := X / Y; -- # no_div_fault req-rule #2
   end;

   --

   procedure Div
     (X, Y : Integer; T : out Integer; Fault : out Boolean)
   is
      CE_Fault, DE_Fault : Boolean;

   begin

      -- req-rule #4

      begin
         Fail_Range_Check; -- # stmt
      exception
         when Constraint_Error => null; -- # stmt
      end;

      begin
         Div_CE (X, Y, T); -- # stmt
         CE_Fault := False; -- # no_div_fault req-rule #2
      exception
         when Constraint_Error =>
            CE_Fault := True;  -- # div_fault req-rule #3 and #5
         when others =>
            null;  -- # bad_handler req-rule #3
      end;

      begin
         Div_DE (X, Y, T); -- # stmt
         DE_Fault := False; -- # no_div_fault req-rule #2
      exception
         when Div_Error =>
            DE_Fault := True;  -- # div_fault req-rule #3 and #5
         when others =>
            null;  -- # bad_handler req-rule #3
      end;

      Fault := CE_Fault or else DE_Fault; -- # stmt
   end;

   --

   pragma Unsuppress (All_Checks);

   procedure Do_Bump (X : in out Integer) is
      One : Integer := 1;
      pragma Volatile (One);
   begin
      -- This will raise constraint_error on integer'last
      X := One * (X + One); -- # do_bump #4
   end;

   procedure Bump (X : in out Integer; Fault : out Boolean) is
   begin
      Do_Bump (X); -- # do_bump
      Fault := False; -- # no_bump_fault  #2
   exception
      when Constraint_Error =>
         Fault := True; -- # bump_fault #3
   end;
end;
