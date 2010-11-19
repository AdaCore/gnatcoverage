with Support;
separate (Pack)
package body Inner is
   function Fun (I : Integer) return Integer is separate;
begin
   I := Support.Identity (1);      -- # stmt
end Inner;
