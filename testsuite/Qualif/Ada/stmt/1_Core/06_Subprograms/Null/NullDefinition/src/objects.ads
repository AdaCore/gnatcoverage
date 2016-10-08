with Support;

package Objects is
   type T_Object is null record;
   
   procedure Probe (O : T_Object) is null; -- # global
      
   -- Some declaration with initialization code here, to verify
   -- that coverage by elaboration doesn't backpropagate:
   
   X : Integer := Support.Identity(54); -- # decl_with_elab
      
   procedure Process (O : T_Object; Local : Boolean);
end;
