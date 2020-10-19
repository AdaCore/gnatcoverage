package body Counters is

   procedure Reset (C : in out Counter) is
   begin
      C.Value := 0;
   end;

   procedure Bump (C : in out Counter) is
   begin
      C.Value := C.Value + 1;
   end;
end;
