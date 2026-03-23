pragma Ada_2012;

package body Lib is
   function First return Integer is (1);
   function Next (I : Integer) return Integer is (I + 1);
end;
