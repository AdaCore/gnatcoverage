
package body Notes is

   function Id (N : Cnote) return Nid is
   begin
      raise Program_Error; -- # cnote
      return "ERR!";       -- no code for this line
   end;

   function Id (N : Dtne) return Nid is
   begin
      return "DTNE"; -- # dtne
   end;

   function Id (N : Dfne) return Nid is
   begin
      return "DFNE";  -- # dfne
   end;

end;
