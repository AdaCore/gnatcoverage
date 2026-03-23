
package Notes is

   type Nid is new String (1 .. 4);


   type Cnote is null record;

   function Id (N : Cnote) return Nid;

   --

   type Dtne is new Cnote;

   function Id (N : Dtne) return Nid;

   --

   type Dfne is new Cnote;

   function Id (N : Dfne) return Nid;

end;
