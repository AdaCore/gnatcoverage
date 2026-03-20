function Proc (AA, BB, Override : Boolean) return Boolean is
   generic
      Result : Boolean := AA and then BB;                 -- # decision
   procedure Get_Result (Value : out Boolean);

   ----------------
   -- Get_Result --
   ----------------

   procedure Get_Result (Value : out Boolean) is
   begin
      Value := Result;                                    -- # stmt
   end Get_Result;

   Result : Boolean;                                      -- # stmt
begin
   if Override then                                       -- # if-override
      declare
         procedure Get is new Get_Result (AA or else BB); -- # inst-override
      begin
         Get (Result);                                    -- # ret-override
      end;
   else
      declare
         procedure Get is new Get_Result;                 -- # inst-default
      begin
         Get (Result);                                    -- # ret-default
      end;
   end if;

   return Result;                                         -- # stmt
end Proc;
