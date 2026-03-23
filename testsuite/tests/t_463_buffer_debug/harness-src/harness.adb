with Ada.Text_IO; use Ada.Text_IO;

with GNATcov_RTS.Buffers;       use GNATcov_RTS.Buffers;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;
with GNATcov_RTS.Strings;       use GNATcov_RTS.Strings;

with Generated;

--  Pull Main in the build closure, so that it is considered as unit of
--  interest even though it is not a main when starting from harness.gpr.

with Main;

procedure Harness is
begin
   for G_Index in Generated.Buffers_Groups'Range loop
      declare
         G       : GNATcov_RTS_Coverage_Buffers_Group
           renames Generated.Buffers_Groups (G_Index).all;
         Buffers : Coverage_Buffers_Group (1 .. Natural (G.Length));
         pragma Import (C, Buffers);
         for Buffers'Address use G.Buffers;

         Unit_Name : GNATcov_RTS_String;
      begin
         --  All buffer groups are supposed to have at least one buffer

         if Buffers'Length = 0 then
            raise Program_Error;
         end if;

         for I in Buffers'Range loop
            declare
               B : GNATcov_RTS_Coverage_Buffers renames Buffers (I).all;

               Name : String (1 .. Natural (B.Unit_Name.Length));
               pragma Import (C, Name);
               for Name'Address use B.Unit_Name.Str;
            begin
               --  Check that all buffers in this group are for the same unit

               if I = 1 then
                  Unit_Name := B.Unit_Name;
                  Put_Line (Name);
               else
                  declare
                     UN : String (1 .. Natural (Unit_Name.Length));
                     pragma Import (C, UN);
                     for UN'Address use Unit_Name.Str;
                  begin
                     if UN /= Name then
                        raise Program_Error;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end;
   end loop;
end Harness;
