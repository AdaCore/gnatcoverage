pragma Ada_2012;

package body Values is

   function All_Pos (S1, S2: Sequence) return Boolean is
   begin
      --  Assertions are not (yet) supposed to be processed by gnatcov,
      --  but this kind of construct can make the instrumenter fail
      --  (see U526-032) so this assertion is here only to test the
      --  instrumenter robustness for the moment.
      --  Coverage results are irrelevant for the time being.

      pragma Assert (for all K in Value_Seq'Range => Value_Seq (K) > 0);

      return (for all K in S1'Range => S1(K) > 0) and then (for all E of S2 => E > 0); -- # eval
   end All_Pos;
end Values;
