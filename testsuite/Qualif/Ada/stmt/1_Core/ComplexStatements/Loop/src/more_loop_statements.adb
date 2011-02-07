with LOOP_Statements_Support; use LOOP_Statements_Support;
with Support;                 use Support;
package body More_LOOP_Statements is
   Idx : Natural;


   function N_Of (L : Level; S : Sample) return Natural is
      I : Integer;
      N : Natural;
   begin
      I := S'First;            -- # preLoop1
      N := 0;                  -- # preLoop1

      while I <= S'Last loop   -- # Loop1
         if S (I) = L then     -- # inLoop1
            N := N + 1;        -- # inIfinLoop1
         end if;

         I := I + 1;           -- # inLoop1
      end loop;

      return N;                -- # postLoop1
   end;

   procedure Change_Char
     (S      : in out String;
      Old_Ch :        Character;
      New_Ch :        Character)
   is
   begin
      for J in S'Range loop       -- # Loop2
         if S (J) = Old_Ch then   -- # inLoop2
            S (J) := New_Ch;      -- # inIfinLoop2
         end if;
      end loop;
   end Change_Char;

begin
   Idx := Identity (1);                              --# elab

   while Idx <= Identity (10) loop                   --# elab
      Global_Sample (Idx) := Level (Identity (Idx)); --# elab
      Idx := Idx + 1;                                --# elab
   end loop;

   for J in Global_String'Range loop                 --# elab
      Global_String (J) := Character'Val (J);        --# elab
   end loop;

end More_LOOP_Statements;

