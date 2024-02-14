
package Values is
   type Num is mod 2**8;
   for Num'Size use 8;

   type Array_Type is array (1 .. 4) of Num;
   pragma Pack (Array_Type);

   procedure Touch (E : in out Num);
end;
