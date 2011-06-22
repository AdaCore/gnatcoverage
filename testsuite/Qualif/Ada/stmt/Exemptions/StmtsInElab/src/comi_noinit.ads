--  Instantiate Gcom with auto-init False. Identity prevents variations in
--  results from possible constant folding otherwise, irrelevant for the test
--  purposes.

with Support, Gcom; use Support;
package Comi_NoInit is new Gcom (Auto_Init => Identity(False));

