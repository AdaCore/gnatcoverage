
package Objects is
   
   N_Positives : Integer := 0;
   
   procedure Characterize (X : Integer);
   pragma No_Return (Characterize);
   
   procedure Proxy_Characterize (X : Integer);

end;
