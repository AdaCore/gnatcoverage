pragma Ada_2022;

package body Pkg is

   function Compute (A, B, C, D , Stop : Boolean) return Boolean is
   begin
      return                                               -- # ret_stmt
        (declare                                           -- # declare
         AB_AT    : constant Boolean := A and then B;      -- # at
         CD_OE    : constant Boolean := C or else D;       -- # oe
         Did_Stop : constant Boolean :=                    -- # stop_stmt
           (if Stop then raise Dummy_Error else Stop);     -- # raise_expr
         F        : Boolean renames B;                     -- # rename
         begin                                             -- # begin
         AB_AT or else CD_OE);                             -- # eval
   end Compute;

end Pkg;
