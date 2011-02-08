package body GOTO_Statements_Block is

   function Compute
     (Par1 : Integer;
      Par2 : Integer;
      Par3 : Integer;
      Par4 : Integer)
      return Integer
   is
      Result : Integer;
   begin

      begin
         Result := Par1 * 100;               -- # 1inblock

         if Result > Integer'Last / 2 then   -- # 1if
            Result := 1;                     -- # in1if
            goto Fin;                        -- # 1goto
         end if;

         begin
            Result := Result + Par2;         -- # 2block
         exception
            when Constraint_Error =>
               if Par4 > 0 then              -- # 2if
                  Result := Par3;            -- # in2if
                  goto Fin;                  -- # 2goto
               end if;

               Result := 100;                -- # after2goto
         end;
      end;

      Result := Result * Par4;               -- # afterblock

      <<Fin>> return Result;                 -- # fin

   end Compute;

end GOTO_Statements_Block;
