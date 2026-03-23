package body FUOR is

   function Oplen (S : String) return Integer is
   begin
      return S'Length; -- # oncall
   end;

   function Postfits_Or
     (S, Post1, Post2 : String; Max : Integer) return Boolean is
   begin
      return Oplen(S & Post1) <= Max -- # evalA
        or else Oplen(S & Post2) <= Max; -- # evalB
   end;
end;
