with Ops;

generic
   type T is private;
   with procedure Touch (Value : in out T) is <>;
package Pack is
   procedure Separate_Subp (Value : in out T);
end Pack;
