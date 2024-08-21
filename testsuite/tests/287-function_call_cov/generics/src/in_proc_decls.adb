pragma Ada_2012;

procedure In_Proc_Decls                   -- # p
is
   Dummy : Integer := 0;                  -- # decl

   --  Generic procedure

   generic
      Type A is private;
   procedure P1 (X : A);

   procedure P1 (X : A) is                -- # fun
   begin
      Dummy := 1;                         -- # stmt
   end P1;

   --  Generic null procedure

   generic
      Type D is private;
   procedure P2 (X : D);

   procedure P2 (X : D) is null;          -- # null_fun

  --  Generic function

   generic
      Type B is private;
   function F1 (X : B) return B;

   function F1 (X : B) return B           -- # fun
   is
      Dummy : B := X;                     -- # stmt
   begin
      return Dummy;                       -- # stmt
   end F1;

   --  Generic expression function

   generic
      Type C is private;
   function F2 (X : C) return C;

   function F2 (X : C) return C is (X);   -- # exp_fun

   --  Instanciations
   --  LIMITATION : function coverage is currently not computed for
   --  instanciations of generic subprograms
   procedure Inst_P1 is new P1 (Integer); -- # stmt
   procedure Inst_P2 is new P1 (Integer); -- # stmt
   function Inst_F1 is new F1 (Integer);  -- # stmt
   function Inst_F2 is new F2 (Integer);  -- # stmt
begin
   Inst_P1 (42);                          -- # cstmt
   Inst_P2 (42);                          -- # cstmt
   Dummy := Inst_F1 (42);                 -- # cstmt
   Dummy := Inst_F2 (42);                 -- # cstmt
end In_Proc_Decls;
