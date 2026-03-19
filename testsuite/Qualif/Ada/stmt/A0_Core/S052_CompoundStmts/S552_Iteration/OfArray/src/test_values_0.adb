with Values.Global, Values.Arg;

procedure Test_Values_0 is
begin
   null;
end;

--# values-global.adb
--  /stmt/    l- ## s-
--  /loop_op/ l- ## s-

--# values-arg.adb
--  /decl/    l- ## s-
--  /test-return/ l- ## s-
--  /return/      l- ## s-
--  /for-stmt/    l- ## s-
--  /test-exit/   l- ## s-
--  /exit/        l- ## s-
--  /loop_op/   l- ## s-
--  /post-loop/ l- ## s-
