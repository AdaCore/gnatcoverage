with Parser; use Parser;
procedure Test_Parser_TF is
begin
   Parser.Parse_Array ((Tok_Stuff, Tok_Stuff, Tok_EOL, Tok_Close_Bracket));
end Test_Parser_TF;

--# parser.adb
--  /append/  l+ ## 0
--  /ex1/     l+ ## 0
--  /ex2/     l! ## dT-
