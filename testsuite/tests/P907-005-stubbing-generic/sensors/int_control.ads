with Temperature_Control;
with Pressure_Control;

package Int_Control is
   package TC is new Temperature_Control (Integer);
   package PC is new Pressure_Control (Integer);
end Int_Control;
