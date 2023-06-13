with Parser; use Parser;
procedure Test_Parser_TT is
begin
   Parser.Parse_Array ((Tok_Stuff, Tok_Stuff, Tok_Stuff, Tok_Close_Bracket));
end Test_Parser_TT;

--# parser.adb
--  /append/  l+ ## 0
--  /ex1/     l! ## dT-
--  /ex2/     l! ## dF-
