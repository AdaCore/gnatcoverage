package Parser is
   type Tok is (Tok_Close_Bracket, Tok_Semicolon, Tok_EOL, Tok_Stuff);
   type Toks is array (Natural range <>) of Tok;

   procedure Parse_Array (T : Toks);
end Parser;
