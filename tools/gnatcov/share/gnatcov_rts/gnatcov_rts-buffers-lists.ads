--  Parent package to host lists of buffers for units of interest

package GNATcov_RTS.Buffers.Lists is

   type Unit_Coverage_Buffers_Access is
      not null access all Unit_Coverage_Buffers;

   type Unit_Coverage_Buffers_Array is
      array (Positive range <>) of Unit_Coverage_Buffers_Access;

end GNATcov_RTS.Buffers.Lists;
