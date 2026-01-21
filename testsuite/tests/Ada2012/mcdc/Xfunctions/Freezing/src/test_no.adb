pragma Ada_2005;

with Pkg.Simple; use Pkg.Simple;

procedure Test_No is
begin
   if Singleton.Is_Null then
      raise Program_Error;
   end if;

   if Singleton.Get_Model.Is_Null then
      raise Program_Error;
   end if;
end Test_No;

--# pkg.ads
--  /get-id-cond/ l- ## s-
--  /get-id-then/ l- ## 0
--  /get-id-else/ l- ## 0

--# pkg.adb
--  /clone/   l- ## s-
--  /is-null/ l+ ## 0
