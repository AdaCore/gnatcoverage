--  Instantiate Gcom with auto-init True.  Identity prevents variations in
--  results from possible constant folding otherwise, irrelevant for the test
--  purposes.

with Support, Gcom; use Support;
package Comi_Init is new Gcom (Auto_Init => Identity(True));
