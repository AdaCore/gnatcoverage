with Foo;

procedure Bar is

   function Fibo (N : Natural) return Natural is

      function Helper (Step, N1, N2 : Natural) return Natural is
      begin
         if Step >= N then
            return N1;
         else
            return Helper(Step + 1, N2, N1 + N2);
         end if;
      end Helper;

   begin
      return Helper (0, 0, 1);
   end Fibo;

   Fibo_Ref : constant array (Natural range <>) of Natural :=
     (0, 1, 1, 2, 3, 5, 8, 13, 21);

begin
   Foo;

   for N in Fibo_Ref'Range loop
      if Fibo (N) /= Fibo_Ref (N) then
         raise Program_Error;
      end if;
   end loop;
end Bar;
