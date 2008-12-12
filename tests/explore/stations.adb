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

      --  Robot_Control / User input associations

      Robot_Control_Map : constant array (Robot_Control) of Character
        := (Nop          => 'N',
            Probe        => 'P',
            Step_Forward => 'S',
            Rotate_Left  => 'L',
            Rotate_Right => 'R');

      C : Character;                -- User input - next op to perform
      Ctrl : Robot_Control := Nop;  -- Associated Robot_Control
   begin

      --  In case something came in since last time ...

      Process_Pending_Inputs (Sta);

      --  Get the next command from the terminal line and map it to an
      --  internal control code.  The internal code will remain a Nop if the
      --  input isn't recognized.

      Put ("'N'op, 'P'robe, 'S'tep, Rotate 'L'eft/'R'ight, 'Q'uit ? ");
      Flush;
      Get (C);

      if C = 'Q' then
         Kill (Sta.All);
         return;
      end if;

      for Candidate in Robot_Control_Map'Range loop
         if C = Robot_Control_Map (Candidate) then
            Ctrl := Candidate;
         end if;
      end loop;

      --  Push the command out to the robot, so that it executes, and follow
      --  with a probe request so that a situation update gets sent if this
      --  is not just what we asked.

      Push (Ctrl, Robot_Control_Outport (Sta.all));
      if Ctrl /= Probe then
         Push (Probe, Robot_Control_Outport (Sta.all));
      end if;

      --  In case the Robot reacts instantly ...

      Process_Pending_Inputs (Sta);

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
