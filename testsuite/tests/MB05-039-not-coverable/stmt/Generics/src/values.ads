generic
   type Value_T is range <>;
   Factor : in Integer;
   
package Values is
   function F (X : in Value_T) return Integer;
end;
