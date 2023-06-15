package New_Alert_System is

   type My_Time is new Natural;
   Zero_Time     : My_Time := My_Time'First;
   Critical_Time : My_Time := 100;

   function My_Clock return My_Time;
   procedure Set_Time (New_Time : My_Time);

   type Device is new Natural;
   System_Device : Device;

   type Person is (Volunteer, Soldier, Sergeant, Major);
   type Text is new String (1 .. 10);
   No_Message : Text := (others => ' ');

   type Alert is tagged record
      Idx             : Natural;
      Time_Of_Arrival : My_Time;
      Message         : Text;
   end record;

   procedure Handle  (A     : in out Alert);

   procedure Display (A     : in  Alert;
                      On    : out Device);

   function Log (A : in Alert) return Natural;


   type Low_Alert is new Alert with null record;

   type Medium_Alert is new Alert with record
      Action_Officer: Person;
   end record;

   -- now override inherited operation
   procedure Handle (MA : in out Medium_Alert);

   type High_Alert is new Medium_Alert with record
      Ring_Alarm_At: My_Time;
   end record;

   procedure Handle (HA : in out High_Alert);
   procedure Set_Alarm (HA : in out High_Alert);
   function Log (HA : in High_Alert) return Natural;


end New_Alert_System;
