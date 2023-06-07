with New_Alert_System.Objects; use New_Alert_System.Objects;

package body New_Alert_System.Emergency is

   procedure Handle (EA: in out Emergency_Alert) is
   begin
      Handle (New_Alert_System.Alert (EA));         -- # ea_handle

      if EA.Time_Of_Arrival >= New_Alert_System.Critical_Time then -- # ea_handle
         EA.Catastrophe := True;                   -- # ea_hand_catastrophe
      else
         EA.Catastrophe := False;                  -- # ea_hand_no_catastrophe
      end if;
   end Handle;

   procedure Display (EA    : in  Emergency_Alert;
                      On    : out Device)
   is
   begin
       if EA.Catastrophe then                      -- # ea_display
          On := 0;                                 -- # ea_displ_catastrophe
       else
          Display (New_Alert_System.Alert (EA), On); -- # ea_displ_no_catastrophe
       end if;
   end Display;

   function Log (EA: in Emergency_Alert) return Natural is
   begin
      return Log (New_Alert_System.Alert (EA)) + 100; -- # ea_log
   end Log;

   function Catastrophe (EA: in Emergency_Alert) return Boolean is
   begin
      return EA.Catastrophe;                          -- # ea_catastrophe
   end Catastrophe;

   function Get_Empty_Emergency_Alert return Emergency_Alert is
   begin
      return Emergency_Alert'(Empty_Alert with Catastrophe => False); -- # ea_get
   end Get_Empty_Emergency_Alert;

end New_Alert_System.Emergency;
