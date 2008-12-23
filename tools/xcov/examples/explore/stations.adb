----------------------------------------------------------------------------
--                             STATIONS (BODY)                            --
----------------------------------------------------------------------------

with Text_IO; use Text_IO;

package body Stations is

   --------------------
   -- IOports access --
   --------------------

   function Robot_Control_Outport
     (Sta : Station) return Robot_Control_Links.IOport_Access is
   begin
      return Sta.Robot_Control_Outp;
   end;

   function Robot_Situation_Inport
     (Sta : Station) return Situation_Links.IOport_Access is
   begin
      return Sta.Robot_Situation_Inp;
   end;

   use Robot_Control_Links, Situation_Links;

   ---------
   -- Run --
   ---------

   procedure Run (Sta : Station_Access) is

      --  Update MAP from the info found in SITU, about the kind of square
      --  probed ahead of a given position.

      procedure Update (Map : in out Geomap; Situ : Situation) is
         Posa : constant Position := Pos_Ahead_Of (Situ);
      begin
         Map (Posa.X, Posa.Y) := Situ.Sqa;
      end;

      --  Fetch and process pending Situation inputs - update and
      --  dump the map of our local view of the field.

      procedure Process_Pending_Inputs (Sta : Station_Access) is
         Situ : Situation;
      begin
         while not Empty (Robot_Situation_Inport (Sta.all)) loop
            Pop (Situ, Robot_Situation_Inport (Sta.all));
            Update (Sta.Map, Situ);
            Dump (Sta.Map, Situ);
         end loop;
      end;

      C : Character;  -- User input - next op to perform

      function Control_For
        (C : Character) return Robot_Control is
      begin
         case C is
            when 'P' =>
              return (Code => Probe, Value => 0);
            when 'S' =>
               return (Code => Step_Forward, Value => 0);
            when 'L' =>
               return (Code => Rotate_Left, Value => 0);
            when 'R' =>
               return (Code => Rotate_Right, Value => 0);
            when 'C' =>
               return (Code => Opmode, Value => Robot_Opmode'Pos (Cautious));
            when 'D' =>
               return (Code => Opmode, Value => Robot_Opmode'Pos (Dumb));
            when others =>
               return (Code => Nop, Value => 0);
         end case;
      end;
   begin

      --  In case something came in since last time ...

      Process_Pending_Inputs (Sta);

      --  Get the next command from the terminal line and map it to an
      --  internal control code.  The internal code will remain a Nop if the
      --  input isn't recognized.

      Put_Line ("'C'autious mode, 'D'umb mode");
      Put ("'P'robe, 'S'tep, Rotate 'L'eft/'R'ight, 'Q'uit ? ");
      Flush;
      Get (C);

      if C = 'Q' then
         Kill (Sta.All);
         return;
      else
         declare
            Ctrl : Robot_Control := Control_For (C);
         begin

            --  Push the command out to the robot, so that it executes, and
            --  follow with a probe request so that a situation update gets
            --  sent if this is not just what we asked.

            Push (Ctrl, Robot_Control_Outport (Sta.all));
            if Ctrl.Code /= Probe then
               Push ((Code => Probe, Value => 0),
                     Robot_Control_Outport (Sta.all));
            end if;

            --  In case the Robot reacts instantly ...

            Process_Pending_Inputs (Sta);
         end;
      end if;
   end;

   ----------
   -- Init --
   ----------

   procedure Init (Sta : Station_Access) is

      procedure All_Unknown (Map : in out Geomap) is
      begin
         for X in Map'range (Sqx) loop
            for Y in Map'Range (Sqy) loop
               Map (X, Y) := Unknown;
            end loop;
         end loop;
      end;

   begin
      Sta.Robot_Control_Outp
        := new Robot_Control_Links.IOport (Capacity => 2,
                                           Owner => Actor_Ref (Sta));
      Sta.Robot_Situation_Inp
        := new Situation_Links.IOport (Capacity => 1,
                                       Owner => Actor_Ref (Sta));
      All_Unknown (Sta.Map);
   end;

end;
