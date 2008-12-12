----------------------------------------------------------------------------
--                                  ACTORS                                --
----------------------------------------------------------------------------

--  This unit exposes the root Actor abstraction for the Explore example,
--  meant to be used for active entities in the system, such as the robot
--  or the control station.
--
--  This is useful to allow associations of arbitrary entities to other
--  software components, for instance to support port/actor ownership.

package Actors is
   type Actor is abstract tagged private;
   type Actor_Ref is access all Actor'Class;

   function Live (A : Actor'Class) return Boolean;
   procedure Kill (A : in out Actor'Class);
private
   type Actor is abstract tagged record
      Live : Boolean := True;
   end record;
end;
