|QA| Cycles
===========

Each |QA| cycle consists of the following iterations:

* The **Qualification Team** starts the cycle by adding the documents
  submitted for review into the ``qa`` area. The **Quality Assurance Team** is
  then notified.

:raw-latex:`\begin{minipage}{\linewidth}`

* The **Quality Assurance Team** identifies defects in the documents. For each
  defect the **Quality Assurance Team** enters in the report a unique *defect
  id*, its *location and scope*, *description*, as well as defect *severity
  code*, which can be:

  * **EDIT** - minor editorial remark.
  * **MINOR** - minor defect, i.e. one that is not blocking the document
      release.
  * **MAJOR** - major defect, i.e. one that is blocking the document release.

  **Qualification Team** is notified when the review is complete.

:raw-latex:`\end{minipage}`

:raw-latex:`\begin{minipage}{\linewidth}`

* The **Qualification Team** reviews each defect, and either Accepts or
  Rejects it.

  * If a defect is **Accepted**: *Corrective Action* is described and applied.
  * If a defect is **Rejected**: *Reason* is provided.

:raw-latex:`\end{minipage}`

* If any defects have been **Accepted** and *Corrective Actions* applied: new
  version of the document(s) is generated and added to the ``qa`` area.

* The **Quality Assurance Team** reviews remarks for **Rejected** defects,
  description and proper application of *Corrective Actions* for **Accepted**
  defects. If the **Quality Assurance Team** agrees with the **Qualification
  Team**'s response to a defect, it is marked as *Resolved*.

* The **Qualification Team** and The **Quality Assurance Team** converge until
  at least all **MAJOR** defects are *Resolved*.

  i.e.: The **Quality Assurance Team** agrees either with the given *Reason*
  if a defect is **Rejected**, or with the proposed *Corrective Action* (and
  its proper application has then been verified) if defect is **Accepted**.

* The documents are then deemed ready for release.

