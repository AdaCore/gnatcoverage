package Doraise is

   User_Error : exception;

   -- How a raise is to be propagated. If not None, this can be
   -- either directly (Straight) or via an intermediate handler that
   -- will re-raise (Reraise_One/Other). For Reraise_One, there is
   -- a specific handler for each exception id (CE, PE, User). For
   -- Reraise_Other, this would be a "when others":

   type Raise_Mode is
     (Straight, Reraise_One, Reraise_Other, None);

   -- The kind of raise operation a testcase can request:

   type Raise_Kind is
     (Implicit_CE, -- implicit Constraint_Error out of a range check
      Explicit_PE, -- explicit raise Constraint_Error stmt
      Explicit_UE, -- explicit raise User_Error stmt
      From_RTS,    -- a Constraint_Error out of a 'Valid call
      None);       -- a sensible choice to go with Raise_Mode None

   procedure Dispatch (Rk : Raise_Kind; Rm : Raise_Mode);

end;
