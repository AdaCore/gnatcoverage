with Example; use Example;

procedure Test_0 is
begin
   null;
end Test_0;

--# example.ads
--%tags: !CARGS_gnat2022
--  /f1/ l? ## s?
--  /f2/ l? ## s?
--
--%tags: CARGS_gnat2022
--  /f1/ l- ## s-
--  /f2/ l- ## s-
--
--# example.adb
--  /root-p/           l- ## s-
--  /child-p/          l- ## s-
--  /not-primitive-p2/ l- ## s-
