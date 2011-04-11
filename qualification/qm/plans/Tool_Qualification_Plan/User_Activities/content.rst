===============
User Activities
===============

In order to finalize the qualification of GNATcoverage, the final user needs to perform a set of activities, either in the scope of GNATcoverage qualification or GNATcoverage usage.

.. rubric:: GNATcoverage Qualification


#. **Reference GNATcoverage in the PSAC.** The user needs to:

   * identify GNATcoverage as a verification tool that needs to be qualified;
   * identify the compilation options for GNAT Pro.

#. **Delivery Acceptance.** On delivery of GNATcoverage and its qualification kit, the user shall assess the representativeness of the verification environment with the operational environment.
#. **Configuration Management of Qualification Data.** The user need to put under configuration management all qualification data, see compliance matrix to table A-8 in :qmref:`$(project)::Plans::Software Configuration Management Plan`.
#. **Tool conformity review.** The user need:
    * To check that the current version of GNATcoverage has been used.
    * To finalize the discussion on open problems (if any).
#. **Provide a tool qualification agreement**, see section 12.2.4 of DO-178B.


.. rubric:: GNATcoverage usage


#. **Tool Installation in Operational Environment.** The user needs to install the tool in the Operational Environment.
#. **Check correct usage of GNATcoverage .** For GNATcoverage results to be used in a certification context, the used tool interface must comply with the GNATcoverage qualified interface described in section 2.4, including the referenced rules file and chosen runtime.
#. **Update Environment Configuration Index.** The delivery file shall be included in the Environment Configuration Index; the Tests Results shall be included as well.
#. **Update the Software Accomplishment Summary (SAS).** The SAS need to be updated:

   * For objective of table A7, objective 5, 6, or 7 depending on the criticality level of the embedded application.
   * For qualification status of GNATcoverage.
