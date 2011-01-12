--  Common bits for functional decision evaluators involving string slices in
--  conditions

package Blocks is

   subtype Block is String (1 .. 10);
   subtype Piece is String (1 .. 3);

   Sample : Block := "1234567890";

   Pre   : Piece := "123";
   Nopre : Piece := "xxx";
   Suf   : Piece := "890";
   Nosuf : Piece := "xxx";
end;
