with Parser; use Parser;
procedure Test_Parser_FX is
begin
   Parser.Parse_Array ((Tok_Stuff, Tok_Semicolon, Tok_Close_Bracket));
end Test_Parser_FX;

--# parser.adb
--  /append/  l+ ## 0
--  /ex1/     l+ ## 0
--  /ex2/     l! ## dT-
