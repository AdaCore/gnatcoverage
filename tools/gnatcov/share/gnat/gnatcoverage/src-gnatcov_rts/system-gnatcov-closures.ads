with System.GNATcov.Buffers;

--  Parent package to host lists of units of interest

package System.GNATcov.Closures is

   type Unit_Coverage_Buffers_Access is
      not null access all System.GNATcov.Buffers.Unit_Coverage_Buffers;

   type Unit_Coverage_Buffers_Array is
      array (Positive range <>) of Unit_Coverage_Buffers_Access;

end System.GNATcov.Closures;
