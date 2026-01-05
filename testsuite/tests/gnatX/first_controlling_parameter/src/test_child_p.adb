with Example; use Example;

procedure Test_Child_P is
   C : Child;
begin
   P (1, C);
end Test_Child_P;

--# example.ads
--
-- /f1/ l? ## s?
-- /f2/ l? ## s?
--
--# example.adb
--
-- /root-p/           l- ## s-
-- /child-p/          l+ ## 0
-- /not-primitive-p2/ l- ## s-
