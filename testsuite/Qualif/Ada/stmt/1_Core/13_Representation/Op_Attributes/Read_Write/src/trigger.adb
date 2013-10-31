with Mystreams; use Mystreams;

package body Trigger is
   
   -- point here is to have calls via operational attributes
   -- conditioned, controlling that they are not marked covered
   -- just because they are special sorts of calls.
   
   procedure Run (Op : Opkind) is
      P : aliased Port_T;
      X : Sint := (Value => 1);
   begin
      case Op is -- # trigger_test
         when Read => Sint'Read (P'Access, X);   -- # trigger_read
         when Write => Sint'Write (P'Access, X); -- # trigger_write
      end case;
   end;
end;
 
