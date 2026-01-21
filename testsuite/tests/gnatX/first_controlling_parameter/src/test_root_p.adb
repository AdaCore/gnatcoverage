with Example; use Example;

procedure Test_Root_P is
   R : Root;
begin
   P (1, R);
end Test_Root_P;

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
--  /root-p/           l+ ## 0
--  /child-p/          l- ## s-
--  /not-primitive-p2/ l- ## s-
