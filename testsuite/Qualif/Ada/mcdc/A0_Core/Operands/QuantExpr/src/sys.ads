pragma Ada_2012;
package Sys is
   type Sensor is record
     I0, I1 : Integer;
   end record;

   type Sensor_Array is array (Natural range <>) of Sensor;

   function Check (SA : Sensor_Array) return Boolean is
        ((for all I in SA'Range => SA(I).I0 > 0 and then SA(I).I1 > 0) -- # forall
         and then  -- # andthen
         (for some S of SA => S.I0 > 10 or else S.I1 > 10)); -- # forsome

   -- The for-all predicate is an and-then
   --
   --  T T T  v0  I0 > 0 , I1 > 0
   --  T F F  v1  I0 > 0 , I1 <= 0
   --  F X F  v2  I0 <= 0

   -- The for-some predicate is an or-else
   --
   --  F F F  v0  I0 <= 10 , I1 <= 10
   --  F T T  v1  I0 <= 10 , I1 > 10
   --  T X T  v2  I0 > 10

   -- See to exercise with inputs driving in combinations of
   -- pairs of vectors. The toplevel decision is an and-then,
   -- so the for-some operand is only evaluated when the for-all
   -- yields True and the combinations of interest are:

   -- for-all  for-some  toplevel consolidation# (test_c#)
   -- v0       v0        T F F    1 2 3 4 5 6
   -- v0       v1        T T T    1 2   4   6
   -- v0       v2        T T T      2     5 6
   -- v1       X         F X F        3 4   6
   -- v2       X         F X F        3   5 6

   -- Rationale for trying sequence #:
   -- test_c1: arbitrary pick of two pairs
   -- test_c2: mcdc on forsome predicate
   -- test_c3: mcdc on forall predicate
   -- test_c4: mcdc on toplevel expression, v0 v1 on both predicates
   -- test_c5: mcdc on toplevel expression, v0 v2 on both predicates
   -- test_c6: mcdc on all expressions

   V0_V0 : Sensor_Array := (1 => (I0 => 9, I1 => 7));
   -- Single sensor, I0 > 0, I1 > 0, I0 <= 10, I1 <= 10

   V0_V1 : Sensor_Array := (1 => (I0 => 1, I1 => 12));
   --  Single sensor, I0 > 0 , I1 > 0 , I0 <= 10 , I1 > 10

   V0_V2 : Sensor_Array := (1 => (I0 => 30, I1 => 2));
   --  Single sensor, I0 > 0 , I1 > 0 , I0 > 10

   V1_X : Sensor_Array := (1 => (I0 => 30, I1 => -1));
   --  Single sensor, I0 > 0 , I1 <= 0

   V2_X : Sensor_Array := (1 => (I0 => -30, I1 => -1));
   --  Single sensor, I0 <= 0

end;
