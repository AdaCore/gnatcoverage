package body Subprogram_Pack is

   procedure Local_Identity_Proc (I : in out Integer);
   function Local_Identity_Fun (I : Integer) return Integer;

   function Fun1 (I : Integer) return Integer is
      Tmp : Integer := I;               -- # fun1
   begin
      Tmp := Local_Identity_Fun (Tmp);  -- # fun1
      Tmp := Tmp - 1;                   -- # fun1
      return Tmp;                       -- # fun1
   end Fun1;

   function Fun2 (I : Integer) return Integer is
      Tmp : Integer := I;               -- # fun2
   begin
      Tmp := Tmp + 2;                   -- # fun2
      return Tmp;                       -- # fun2
   end Fun2;

   procedure Proc1 (I : in out Integer) is
   begin
      I := I - 3;                       -- # proc1
   end Proc1;

   procedure Proc2 (J : out Integer; I : Integer := Fun2 (1)) is
   begin
      J := I + 4;                       -- # proc2
      Local_Identity_Proc (J);          -- # proc2
   end Proc2;

   procedure Proc3 (I : in out Integer) is
   begin
      I := 2 * I;                       -- # proc3
   end Proc3;

   procedure Proc4 (I : in out Integer) is
   begin
      I := 3 * I;                       -- # proc4
   end Proc4;

   procedure Local_Identity_Proc (I : in out Integer) is
      Tmp : Integer := I;               -- # local_proc
   begin
      Tmp := Tmp * 2;                   -- # local_proc
      I   := Tmp / 2;                   -- # local_proc
   end Local_Identity_Proc;

   function Local_Identity_Fun (I : Integer) return Integer is
      Res : Integer := I;               -- # local_fun
   begin
      return Res;                       -- # local_fun
   end Local_Identity_Fun;
end Subprogram_Pack;
