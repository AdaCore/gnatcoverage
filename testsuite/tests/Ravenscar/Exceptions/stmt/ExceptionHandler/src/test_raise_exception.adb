with Support; use Support;
with P; use P;
with Handle_Exception;
procedure test_raise_exception is
begin
   Handle_Exception;

   Assert (Correct_Exception_Propagated = True);
   Assert (Wrong_Exception_Propagated = False);

end;

--# handle_exception.adb
-- /raise/   l+ ## 0
-- /handled/ l+ ## 0
-- /mis_handled/ l- ## s-
--# raise_exception.adb
-- /force_exception/ l+ ## 0
-- /raise/   l+ ## 0
-- /wrong_exception/  l- ## s-
