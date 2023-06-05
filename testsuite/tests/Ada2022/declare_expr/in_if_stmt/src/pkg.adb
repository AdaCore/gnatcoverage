pragma Ada_2022;

package body Pkg is

   function Compute (A, B, C, D , Stop : Boolean) return Boolean is
      Res : Boolean := A;                                -- # before
   begin
      if (declare                                        -- # if_stmt
          AB_AT    : constant Boolean := A and then B;   -- # at
          CD_OE    : constant Boolean := C or else D;    -- # oe
          Did_Stop : constant Boolean :=                 -- # stop_stmt
            (if Stop then raise Dummy_Error else Stop);  -- # raise_expr
          F        : Boolean renames B;                  -- # rename
          begin                                          -- # begin
          AB_AT or else CD_OE)                           -- # eval
      then
         Res := True;                                    -- # after_expr
      end if;
      return Res;                                        -- # after_expr
   end Compute;

end Pkg;
