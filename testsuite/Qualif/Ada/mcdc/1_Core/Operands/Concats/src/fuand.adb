package body FUAND is

   function Oplen (S : String) return Integer is
   begin
      return S'Length; -- # oncall
   end;

   function Postfits_And
     (S, Post1, Post2 : String; Max : Integer) return Boolean is
   begin
      return Oplen(S & Post1) <= Max -- # evalA
        and then Oplen(S & Post2) <= Max; -- # evalB
   end;
end;
