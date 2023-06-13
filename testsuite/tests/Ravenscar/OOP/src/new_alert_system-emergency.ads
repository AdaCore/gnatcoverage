with New_Alert_System;
package New_Alert_System.Emergency is
   type Emergency_Alert is new New_Alert_System.Alert with private;

   procedure Handle (EA: in out Emergency_Alert);

   procedure Display (EA    : in  Emergency_Alert;
                      On    : out Device);

   function Log (EA: in Emergency_Alert) return Natural;

   function Catastrophe (EA: in Emergency_Alert) return Boolean;

   function Get_Empty_Emergency_Alert return Emergency_Alert;

private
   type Emergency_Alert is new New_Alert_System.Alert with record
      Catastrophe : Boolean;
   end record;
end New_Alert_System.Emergency;
