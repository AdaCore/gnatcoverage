----------------------------------------------------------------------------
--                             ROBOTS.DEVICES (BODY)                      --
----------------------------------------------------------------------------

--  This is a fake implementation of the Robots.Devices for the Explore
--  example, allowing simple simulations without real hardware.

package body Robots.Devices is

   --  We construct and maintain an artificial map, force an initial
   --  situation and keep track of it as part of the engine commands
   --  simulation.

   type Hardware is record
      Map  : Geomap;
      Situ : Situation;
   end record;

   procedure Fake_Map (Map : in out Geomap);
   --  Build a fake field MAP to perform pretended probes from, with
   --  block borders and a few blocks within.

   procedure Probe_Ahead (Situ : in out Situation; Map : Geomap);
   --  Fill SITU.Sqa with the square in MAP ahead the location SITU
   --  designates.

   -----------
   -- Probe --
   -----------

   procedure Probe
     (Situ : out Situation; H : Hardware_Access) is
   begin
      --  The current situation is updated by every op execution, so ...

      Situ := H.Situ;
   end;

   -----------------
   -- Probe_Ahead --
   -----------------

   procedure Probe_Ahead (Situ : in out Situation; Map : Geomap) is
      Posa : constant Position := Pos_Ahead_Of (Situ);
   begin
      Situ.Sqa := Map (Posa.X, Posa.Y);
   end;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Ctrl : Robot_Control; H : Hardware_Access) is
   begin
      --  If the engine is asked to push the robot into a rock block,
      --  the robot dies.

      if Ctrl = Step_Forward and then H.Situ.Sqa = Block then
         Dump (H.Map, H.Situ);
         raise Program_Error;
      end if;

      --  Otherwise, just pretend a real engine acted as requested
      --  by updating the position/direction accordingly ...

      case Ctrl is
         when Nop => null;

         when Rotate_Left =>
            if H.Situ.Dir = Direction'First then
               H.Situ.Dir := Direction'Last;
            else
               H.Situ.Dir := Direction'Pred (H.Situ.Dir);
            end if;

         when Rotate_Right =>
            if H.Situ.Dir = Direction'Last then
               H.Situ.Dir := Direction'First;
            else
               H.Situ.Dir := Direction'Succ (H.Situ.Dir);
            end if;

         when Step_Forward =>
            H.Situ.Pos := Pos_Ahead_Of (H.Situ);

         when others =>
            raise Program_Error;
      end case;

      --  And keep the full situation up to date by fetching the
      --  new Square ahead.

      Probe_Ahead (H.Situ, H.Map);
   end;

   --------------
   -- Fake_Map --
   --------------

   procedure Fake_Map (Map : in out Geomap) is

      procedure Block_Borders_On (Map : in out Geomap) is
         X, Y : Natural;
      begin
         X := Map'First(Sqx);
         for Y in Map'Range (Sqy) loop
            Map (X, Y) := Block;
         end loop;
         X := Map'Last(Sqx);
         for Y in Map'Range (Sqy) loop
            Map (X, Y) := Block;
         end loop;

         Y := Map'First(Sqy);
         for X in Map'Range (Sqx) loop
            Map (X, Y) := Block;
         end loop;
         Y := Map'Last(Sqy);
         for X in Map'Range (Sqx) loop
            Map (X, Y) := Block;
         end loop;
      end;

      procedure Clear_Playzone_On (Map : in out Geomap) is
      begin
         for X in Map'First (Sqx) + 1 .. Map'Last(Sqx) - 1 loop
            for Y in Map'First (Sqy) + 1 .. Map'Last(Sqy) - 1 loop
               Map (X, Y) := Clear;
            end loop;
         end loop;
      end;

      procedure Setup_Blocks_On (Map : in out Geomap) is
         X, Y : Natural;
         Stepx : Natural := Map'Length (Sqx) / 5;
         Stepy : Natural := Map'Length (Sqy) / 5;
      begin
         Y := Map'First(Sqy) + Stepy;
         while Y <= Map'Last(Sqy) loop
            X := Map'First(Sqx) + Stepx;
            while X <= Map'Last(Sqx) loop
               Map (X, Y) := Block;
               X := X + Stepx;
            end loop;
            Y := Y + Stepy;
         end loop;
      end;

   begin
      Block_Borders_On (Map);
      Clear_Playzone_On (Map);
      Setup_Blocks_On (Map);
   end;

   ----------
   -- Init --
   ----------

   procedure Init (H : out Hardware_Access) is
   begin
      --  Initialize the pretended devices: build a fake map, force or
      --  initial position to a known-to-be-clear spot, then fill our
      --  current situation Sqa.

      H := new Hardware;

      Fake_Map (H.Map);

      H.Situ.Pos.X := 2;
      H.Situ.Pos.Y := 2;
      H.Situ.Dir := East;

      Probe_Ahead (H.Situ, H.Map);
   end;

end;
