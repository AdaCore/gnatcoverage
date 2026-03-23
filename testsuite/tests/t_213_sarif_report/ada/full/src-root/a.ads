pragma Ada_2012;

package A is
  procedure A_Proc (Dummy : Boolean);

  type T is tagged null record;

  function Create return T is (null record);
end A;
