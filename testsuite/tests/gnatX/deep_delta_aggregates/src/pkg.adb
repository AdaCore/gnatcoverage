pragma Extensions_Allowed (On);

package body Pkg is

   -----------
   -- Set_X --
   -----------

   procedure Set_X
     (A, B : Boolean; Value : Integer; Negate : Boolean) is
   begin
      S :=                                        -- # stmt
        (S with delta                             -- # src
         (A and then B).X                         -- # index
         => (if Negate then -Value else Value));  -- # value
   end Set_X;

end Pkg;
