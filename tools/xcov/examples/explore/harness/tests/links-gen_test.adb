------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with AUnit.Assertions; use AUnit.Assertions;

package body Links.Gen_Test is

   procedure Set_Up (T : in out Test) is
   begin
      if T.Port0 = null then
         T.Port0 := Links.Create_IOport (0, null);
         T.Port1 := Links.Create_IOport (1, null);
         T.Port4 := Links.Create_IOport (4, null);
         T.Link  := new Links.IOlink;
         T.Inp   := Links.Create_IOport (1, Act1);
         T.Outp  := Links.Create_IOport (1, Act2);

      else
         --  Reinitialize the queues.
         declare
            Val : Data_Type;
         begin
            while not Empty (T.Port1) loop
               Pop (Val, T.Port1);
            end loop;
            while not Empty (T.Port4) loop
               Pop (Val, T.Port4);
            end loop;
            while not Empty (T.Inp) loop
               Pop (Val, T.Inp);
            end loop;
            while not Empty (T.Outp) loop
               Pop (Val, T.Outp);
            end loop;
         end;
      end if;
   end Set_Up;

   procedure Test_Full  (T : in out Test) is
   begin
      Assert
        (Full (T.Port0),
         "Port with a capacity of 0 should always be full");
      Assert
        (not Full (T.Port1),
         "Port with a capacity > 0 should not be full by default");
      Assert
        (not Full (T.Port4),
         "Port with a capacity > 0 should not be full by default");

      Push (Data_Val1, T.Port1);
      Assert
        (Full (T.Port1),
         "Port with 'capacity' pushed items should be full");

      Push (Data_Val1, T.Port4);
      Assert
        (not Full (T.Port4),
         "Port with less that 'capacity' pushed items should not be full");
   end Test_Full;

   procedure Test_Empty (T : in out Test) is
   begin
      Assert
        (Empty (T.Port0),
         "Port with a capacity of 0 should always be empty");
      Assert
        (Empty (T.Port1),
         "Port with a capacity > 0 should be empty by default");
      Assert
        (Empty (T.Port4),
         "Port with a capacity > 0 should be empty by default");

      Push (Data_Val1, T.Port1);
      Assert
        (not Empty (T.Port1),
         "Port with 'capacity' pushed items should not be empty");

      Push (Data_Val1, T.Port4);
      Assert
        (not Empty (T.Port4),
         "Port with less that 'capacity' pushed items should not be empty");
   end Test_Empty;

   procedure Test_Pop   (T : in out Test) is
   begin
      Assert (False, "Test_Pop: tbd");
   end Test_Pop;

   procedure Test_Push  (T : in out Test) is
   begin
      Assert (False, "Test_Push: tbd");
   end Test_Push;

   procedure Test_Owner (T : in out Test) is
   begin
      Assert
        (Owner (T.Inp) = Act1,
         "The owner shall be the one used when initializing the port");
      Assert
        (Owner (T.Outp) = Act2,
         "The owner shall be the one used when initializing the port");
   end Test_Owner;

   procedure Test_Connect (T : in out Test) is
      D : Data_Type;
   begin
      Connect (T.Outp, T.Inp, T.Link);

      Assert
        (Empty (T.Outp), "Output port shall be empty at initialisation");
      Assert
        (Empty (T.Inp), "Input port shall be empty at initialisation");

      Push (Data_Val1, T.Outp);

      Assert
        (Empty (T.Outp),
         "Once connected, an output port shall be empty after a push");
      Assert
        (not Empty (T.Inp),
         "The Input port should not be empty after a push to output");

      Pop (D, T.Inp);

      Assert
        (D = Data_Val1,
         "The value received by the input port shall be the same as the one" &
         " sent to the output port");

      Push (Data_Val2, T.Outp);
      Pop (D, T.Inp);

      Assert
        (D = Data_Val2,
         "The value received by the input port shall be the same as the one" &
         " sent to the output port");

      Assert
        (Empty (T.Outp),
         "Output port shall be empty after sending messages");
      Assert
        (Empty (T.Inp),
         "Input port shall be empty after all messages are read");
   end Test_Connect;

end Links.Gen_Test;
