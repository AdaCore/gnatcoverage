------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with Text_IO; use Text_IO;

package body Stations is

   --------------------
   -- IOports access --
   --------------------

   function Robot_Control_Outport
     (Sta : Station) return Robot_Control_Links.IOport_Access is
   begin
      return Sta.Robot_Control_Outp;
   end Robot_Control_Outport;

   function Robot_Situation_Inport
     (Sta : Station) return Situation_Links.IOport_Access is
   begin
      return Sta.Robot_Situation_Inp;
   end Robot_Situation_Inport;

   use Robot_Control_Links, Situation_Links;

   ---------
   -- Run --
   ---------

   procedure Run (Sta : Station_Access) is

      procedure Update (Map : in out Geomap; Situ : Situation);
      --  Update MAP from the info found in SITU, about the kind of square
      --  probed ahead of a given position.

      procedure Update (Map : in out Geomap; Situ : Situation) is
         Posa : constant Position := Pos_Ahead_Of (Situ);
      begin
         Map (Posa.X, Posa.Y) := Situ.Sqa;
      end Update;

      procedure Process_Pending_Inputs (Sta : Station_Access);
      --  Fetch and process pending Situation inputs - update and
      --  dump the map of our local view of the field.

      procedure Process_Pending_Inputs (Sta : Station_Access) is
         Situ : Situation;
      begin
         --  Fetch robot situation inputs and update our local map
         --  accordingly.

         while not Empty (Robot_Situation_Inport (Sta.all)) loop
            Pop (Situ, Robot_Situation_Inport (Sta.all));
            Update (Sta.Map, Situ);

            --  Assume the robot was on solid ground unless we already
            --  knew otherwise.

            if Sta.Map (Situ.Pos.X, Situ.Pos.Y) = Unknown then
               Sta.Map (Situ.Pos.X, Situ.Pos.Y) := Ground;
            end if;

            Dump (Sta.Map, Situ);
         end loop;

      end Process_Pending_Inputs;

      C : Character;  -- User input - next op to perform

      function Control_For (C : Character) return Robot_Control;
      --  Map user input character C to Robot_Control command, Nop if
      --  the input isn't recognized.

      function Control_For
        (C : Character) return Robot_Control is
      begin
         case C is
            when 'p' | 'P' =>
               return (Code => Probe, Value => 0);
            when 's' | 'S' =>
               return (Code => Step_Forward, Value => 0);
            when 'l' | 'L' =>
               return (Code => Rotate_Left, Value => 0);
            when 'r' | 'R' =>
               return (Code => Rotate_Right, Value => 0);
            when 'c' | 'C' =>
               return (Code => Opmode, Value => Robot_Opmode'Pos (Cautious));
            when 'd' | 'D' =>
               return (Code => Opmode, Value => Robot_Opmode'Pos (Dumb));
            when others =>
               return (Code => Nop, Value => 0);
         end case;
      end Control_For;
   begin

      --  In case something came in since last time ...

      Process_Pending_Inputs (Sta);

      --  Get the next command from the terminal line and map it to an
      --  internal control code. Fake echo-on.

      New_Line;
      Put_Line ("'C'autious mode, 'D'umb mode");
      Put_Line ("'P'robe, 'S'tep, Rotate 'L'eft/'R'ight, 'Q'uit ? ");

      Get (C); Put (C); New_Line;

      if C = 'Q' or else C = 'q' then
         Kill (Sta.all);
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
               Push (Control_For ('P'), Robot_Control_Outport (Sta.all));
            end if;

            --  In case the Robot reacts instantly ...

            Process_Pending_Inputs (Sta);
         end;
      end if;
   end Run;

   ----------
   -- Init --
   ----------

   procedure Init (Sta : Station_Access) is

      procedure All_Unknown (Map : in out Geomap);
      --  Fill all the squares in MAP as Unknown

      procedure All_Unknown (Map : in out Geomap) is
      begin
         for X in Map'Range (Sqx) loop
            for Y in Map'Range (Sqy) loop
               Map (X, Y) := Unknown;
            end loop;
         end loop;
      end All_Unknown;

   begin
      Sta.Robot_Control_Outp :=
        Robot_Control_Links.Create_IOport
         (Capacity => 2,
          Owner => Actor_Ref (Sta));
      Sta.Robot_Situation_Inp :=
        Situation_Links.Create_IOport
         (Capacity => 1,
          Owner => Actor_Ref (Sta));
      All_Unknown (Sta.Map);
   end Init;

end Stations;
