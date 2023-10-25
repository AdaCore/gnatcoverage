#if Log then
with Ada.Text_IO; use Ada.Text_IO;
#end if;

package body VM is

   procedure Eval
     (Program : Program_Type;
      PC      : in out PC_Type;
      Stack   : in out Stack_type;
      SP      : in out SP_Type)
   is
      Continue : Boolean := True;

      function Pop return Integer;
      procedure Push (Value : Integer);

      ---------
      -- Pop --
      ---------

      function Pop return Integer is
      begin
#if Log then
         Put_Line ("Popping the stack");
#end if;
         SP := SP - 1;
#if Log then
         Put_Line ("SP:" & SP_Type'Image (SP));
#end if;
         return Stack (SP);
      end Pop;

      ----------
      -- Push --
      ----------

      procedure Push (Value : Integer) is
      begin
#if Log then
         Put_Line ("Pushing the stack");
#end if;
         Stack (SP) := Value;
         SP := SP + 1;
#if Log then
         Put_Line ("SP:" & SP_Type'Image (SP));
#end if;
      end Push;

   begin

#if Log then
      Put_Line ("Program starting:");
      Put_Line ("PC:" & PC_Type'Image (PC));
      Put_Line ("SP:" & SP_Type'Image (SP));
      New_Line;
#end if;

      while Continue loop
         declare
            Inst    : Instruction_Type renames Program (PC);
            Next_PC : PC_Type := PC + 1;
         begin
#if Log then
            Put_Line
              ("Execute: "
               & Opcode'Image (Inst.Kind)
               & " at" & PC_Type'Image (PC));
#end if;
            case Inst.Kind is
               when Halt =>
                  Continue := False;

               when Jump =>
                  Next_PC := Inst.Jump_Dest;

               when Branch =>
                  if Pop /= 0 then
                     Next_PC := Inst.Jump_Dest;
                  end if;

               when Push_Lit =>
                  Push (Inst.Push_Value);

               when Clone =>
                  declare
                     Value : constant Integer := Pop;
                  begin
                     Push (Value);
                     Push (Value);
                  end;

               when Add =>
                  Push (Pop + Pop);
            end case;
            PC := Next_PC;
         end;
      end loop;

#if Log then
      New_Line;
      Put_Line ("Program stopped");
      Put_Line ("PC:" & PC_Type'Image (PC));
      Put_Line ("SP:" & SP_Type'Image (SP));
#end if;
   end Eval;

   ----------
   -- Eval --
   ----------

   function Eval
     (Program        : Program_Type;
      Stack_Size     : Natural;
      Initial_Values : Stack_Type) return Integer
   is
      SP_First : constant SP_Type := Initial_Values'First;
      SP_Last  : constant SP_Type :=
        Initial_Values'First + SP_Type (Stack_Size) - 1;

      Stack : Stack_Type (SP_First .. SP_Last);
      PC    : PC_Type := Program'First;
      SP    : SP_Type := Initial_Values'Last + 1;
   begin
      Stack (Initial_Values'Range) := Initial_Values;
      Eval (Program, PC, Stack, SP);
      return Stack (SP - 1);
   end Eval;

end VM;
