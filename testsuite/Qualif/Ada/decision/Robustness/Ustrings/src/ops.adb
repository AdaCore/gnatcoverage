with Ada.Strings.Maps; use Ada.Strings.Maps;
with Support; use Support;

package body Ops is

   procedure Tryme (A : String; N : Natural; B : String; X : Integer) is
   begin
      declare
         Seq : Unbounded_String :=  A & "*" & Trim(To_Unbounded_String(Integer'Image(N)), Both); -- # tryme
      begin
         if Length (N * A) > 20 then -- # test-len
            Last_Long := Seq; -- # long
         else
            Last_Short := Seq; -- # short
         end if;
      end;

      if X > 0 then -- # test-xpos
         N_Xpos := N_Xpos + 1; -- # xpos

         if B'Length > 0 and then A'Length > 0 and then X * (A & B) = To_Unbounded_String ("ABAB")  -- # comp-abx
         then
            Last_Mapped := Translate (X * B, To_Mapping("ABC", "123")); -- # abab
         end if;

      elsif X < 0 then -- # test-xneg
         N_Xneg := N_Xneg + 1; -- # xneg
      else
         N_Xzero := N_Xzero + 1; -- # xzero
      end if;

   end;

   procedure Check (Which : Last_Category; S : String) is
      Ref : Unbounded_String;
   begin
      case Which is
         when Long =>  Ref := Last_Long; -- # check-long
         when Short => Ref := Last_Short; -- # check-short
         when Mapped => Ref := Last_Mapped; -- # check-mapped
      end case;

      Assert (To_String (Ref) = S); -- # do-check
   end;

end;
