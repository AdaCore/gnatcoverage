pragma Ada_2012;

with Pkg; use Pkg;

procedure Make_Calls                      -- # test
is
   --  LIMITATION : Instances of generic subprograms
   --  Function coverage is currently not computed for instances of generic
   --  subprograms.

   procedure Inst_P1 is new P1 (Integer); -- # stmt
   procedure Inst_P2 is new P1 (Integer); -- # stmt
   function Inst_F1 is new F1 (Integer);  -- # stmt
   function Inst_F2 is new F2 (Integer);  -- # stmt

begin
   Inst_P1 (42);                          -- # call
   Inst_P2 (42);                          -- # call
   Dummy := Inst_F1 (42);                 -- # call
   Dummy := Inst_F2 (42);                 -- # call
end Make_Calls;
