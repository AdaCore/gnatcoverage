package Counters is
   type Counter is record
      Value : Integer := 0;
   end record;

   procedure Reset (C : in out Counter);
   procedure Bump (C : in out Counter);
end;
