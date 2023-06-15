package body Parser is

   procedure Skip_Line_Sep is null;
   procedure Append is null;  -- # append

   procedure Parse_Array (T : Toks) is
      TI : Natural := T'First;

      function Scan_Tok return Tok is
         Result : Tok := T (TI);
      begin
         TI := TI + 1;
         return Result;
      end Scan_Tok;

   begin
      loop
         --  Skip ';' or EOL
         Skip_Line_Sep; -- # stmt

         --  If ']' is alone on its line
         exit when Scan_Tok = Tok_Close_Bracket; -- # ex1

         Append; -- # append
         exit when Scan_Tok /= Tok_Semicolon and then Scan_Tok /= Tok_EOL; -- # ex2
      end loop;
   end Parse_Array;

end Parser;
