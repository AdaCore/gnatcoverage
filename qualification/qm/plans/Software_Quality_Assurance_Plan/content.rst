===============================
Software Quality Assurance Plan
===============================

We remind here that GNATcoverage is qualified as a verification tool. As such such most configuration management activities are not mandatory, in particular:

.. csv-table:: Compliance matrix for Table A-9 of DO-178B
   :delim: |
   :header: "Item", "Description", "Ref.", "Notes", "Activity"

   1|Assurance is obtained that software development and integral processes comply with approved software plans and standards.|8.1a|For verification tools, this is limited to the compliance of tool processes with approved plans|Inspection by sampling of TOR, Test cases and Test results
   2|Assurance is obtained that transition criteria for the software life cycle processes are satisfied.|8.1b|Not required for verification tools|n/a
   3|Software conformity review is conducted.|8.1c, 8.3a, 8.3b, 8.3c | Items 8.3d, 8.3e and 8.3h are not required for CC2; items 8.3f, 8.3g and 8.3i are not required for verification tools|Tool Conformity Review, to be performed by applicant, see :qmref:`$(project)::Plans::Tool Qualification Plan::User_Activities`
