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

with System;
with AUnit.Assertions;      use AUnit.Assertions;

package body Links.Gen_Test is

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Test) is
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
   end Set_Up;

   ---------------
   -- Test_Full --
   ---------------

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

   ----------------
   -- Test_Empty --
   ----------------

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

   --------------
   -- Test_Pop --
   --------------

   procedure Test_Pop   (T : in out Test)
   is
      D : Data_Type;

   begin
      Push (Data_Val1, T.Port1);
      Pop (D, T.Port1);
      Assert
        (D = Data_Val1,
         "Poped value after a push is not the same");

      Push (Data_Val1, T.Port4);
      Push (Data_Val2, T.Port4);
      Pop (D, T.Port4);
      Assert
        (D = Data_Val1,
         "Poped value after a two pushes is not the same");
      Pop (D, T.Port4);
      Assert
        (D = Data_Val2,
         "Second poped value after a two pushes is not the same");
   end Test_Pop;

   ---------------
   -- Test_Push --
   ---------------

   procedure Test_Push  (T : in out Test) is

      Tested_Port : Links.IOport_Access := null;
   begin
      Push (Data_Val1, T.Port1);
      Assert (not Empty (T.Port1), "Port should not be empty after a push");
      Assert (Full (T.Port1), "Port should be full after N 'capacity' push");
   end Test_Push;

   ----------------
   -- Test_Owner --
   ----------------

   procedure Test_Owner (T : in out Test) is
   begin
      Assert
        (Owner (T.Port0) = null,
         "The owner shall be the one used when initializing the port");
      Assert
        (Owner (T.Inp) = Act1,
         "The owner shall be the one used when initializing the port");
      Assert
        (Owner (T.Outp) = Act2,
         "The owner shall be the one used when initializing the port");
   end Test_Owner;

   ------------------
   -- Test_Connect --
   ------------------

   procedure Test_Connect (T : in out Test) is
      D : Data_Type;
   begin
      Connect (T.Outp, T.Inp, T.Link);

      Assert
        (Empty (T.Outp), "Output port shall be empty after initialisation");
      Assert
        (Empty (T.Inp),  "Input port shall be empty after initialisation");

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

--     --------------------
--     -- Test_Pop_Raise --
--     --------------------
--
--     procedure Test_Pop_Raise (T : in out Test)
--     is
--        Tested_Port : Links.IOport_Access := null;
--        D           : Data_Type;
--
--        procedure Pop_Raise;
--        --  Pop on Tested_Port with expected exception raised
--
--        ----------------
--        -- Pop_Raise --
--        ----------------
--
--        procedure Pop_Raise is
--        begin
--           Pop (D, Tested_Port);
--        end Pop_Raise;
--
--        procedure Test_Pop_Raise is new Assert_Exception (Pop_Raise);
--
--     begin
--        Tested_Port := T.Port0;
--        Test_Pop_Raise
--          ("Pop with 0 capacity should always raise an exception");
--
--        Tested_Port := T.Port1;
--        Test_Pop_Raise
--          ("Pop on an empty port should raise an exception");
--
--        Push (Data_Val1, T.Port1);
--        Pop (D, T.Port1);
--        Tested_Port := T.Port1;
--        Test_Pop_Raise
--          ("Calling two pops with a single pushed value should raise " &
--           "an exception");
--     end Test_Pop_Raise;
--
--     ---------------------
--     -- Test_Push_Raise --
--     ---------------------
--
--     procedure Test_Push_Raise (T : in out Test)
--     is
--        Tested_Port : Links.IOport_Access := null;
--
--        procedure Push_Raise;
--        --  Push on Tested_Port with expected exception raised
--
--        ----------------
--        -- Push_Raise --
--        ----------------
--
--        procedure Push_Raise is
--        begin
--           Push (Data_Val1, Tested_Port);
--        end Push_Raise;
--
--        procedure Test_Push_Raise is new Assert_Exception (Push_Raise);
--
--     begin
--        Tested_Port := null;
--        Test_Push_Raise
--          ("Push on a null port should raise an exception");
--
--        Tested_Port := T.Port0;
--        Test_Push_Raise
--          ("Push on a port with 0 capacity should raise an exception");
--
--        Tested_Port := T.Port1;
--        Push (Data_Val1, T.Port1);
--        Test_Push_Raise
--          ("Push on a full port should raise an exception");
--     end Test_Push_Raise;

end Links.Gen_Test;
