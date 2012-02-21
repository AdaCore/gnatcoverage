===============
User Activities
===============

In order to finalize the qualification of GNATcoverage, the final user needs to perform a set of activities, either in the scope of GNATcoverage qualification or GNATcoverage usage.

.. rubric:: GNATcoverage Qualification


#. **Reference GNATcoverage in the PSAC.** The user needs to:

   * identify GNATcoverage as a verification tool that needs to be qualified;
   * identify the compilation options for GNAT Pro.

#. **Delivery Acceptance.** On delivery of GNATcoverage and its qualification kit, the user shall assess the representativeness of the verification environment with the operational environment.

#. **Configuration management of qualification data**. The user need to put under configuration management all qualification data, see :qmref:`$(project)::Plans::Software Configuration Management Hints` for some suggestions.

#. **Provide a tool qualification agreement**, see section 12.2.4 of DO-178B.

#. **Update Environment Configuration Index.** The delivery file shall be included in the Environment Configuration Index; the Tests Results shall be included as well.
#. **Update the Software Accomplishment Summary (SAS).** The SAS need to be updated:

   * For objective of table A7, objective 5, 6, or 7 depending on the criticality level of the embedded application.
   * For qualification status of GNATcoverage.

.. rubric:: GNATcoverage usage


#. **Tool Installation in Operational Environment.** The user needs to install the tool in the Operational Environment.
#. **Check correct usage of GNATcoverage .** For GNATcoverage results to be used in a certification context, the used tool interface must comply with the GNATcoverage qualified interface.
