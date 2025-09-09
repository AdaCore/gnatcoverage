Compliance With Guidance
========================

This section contains the compliance matrices with the guidance contained in
section 12.2 of [|tool_standard|] (or section 4.4(e) of [|tool_supplement_standard|], as appropriate)
and section 11.3.2 of [|tool_supplement_standard|].

.. tabularcolumns:: |p{0.15\linewidth}|p{0.10\linewidth}|p{0.65\linewidth}|

.. csv-table:: Compliance with Section 12.2 of |tool_standard| or section 4.4(e) of |tool_supplement_standard|, as appropriate
   :delim: #
   :header: "Section", "Achieved", "Notes"

   12.2.1 or 4.4(e) #Yes#GNATcoverage qualification is needed. See :ref:`certification-credit`.
   12.2.2 #Yes#GNATcoverage impact corresponds to **Criteria 3**. According to Table 12-1 Tool Qualification Level is **TQL-5** for all assurance levels. See :ref:`tql`.
   4.4(e) #Yes#GNATcoverage is a tool that cannot introduce an error in the output of the qualified tool, but may fail to detect an error in the tool life cycle data, hence the applicable TQL is **TQL-5**. See :ref:`tql`

.. tabularcolumns:: |p{0.15\linewidth}|p{0.18\linewidth}|p{0.60\linewidth}|

.. csv-table:: Compliance with Table 11-1 of |tool_supplement_standard|
   :delim: #
   :header: "Section", "Achieved", "Notes"

   T-0, Objectives 1, 3, 6, 7 # Not applicable # Apply to Tool User, not to Tool Developer per section 11.3 of |tool_supplement_standard|.
   T-0, Objective 2 # Yes # See the requirement description items in the |tor_abb| document.
   T-0, Objective 4 # Not Applicable # Not required for TQL-5. Nevertheless, see :ref:`tqap` for AdaCore's QA procedures.
   T-0, Objective 5 # Yes # See the test and test-cases description items in the |tor_abb| document.
   T-1 to T-7 # Not Applicable # Not required for TQL-5.
   T-8 # Up to the applicant # See :ref:`user-conf-management` for suggested configuration items.
   T-9 # Yes # See the |tqa_doc| document accompanying this qualification package.
   T-10 # Up to the applicant #  Applies to Tool User, not to Tool Developer per section 11.3 of |tool_supplement_standard|.
