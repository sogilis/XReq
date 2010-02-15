--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Stanzas is

   -------------------------------------
   --  Stanza_Type  --  Stanza_Given  --
   -------------------------------------

   function Stanza_Given (S : in String) return Stanza_Type is begin
      return Stanza_Type'(Prefix => Prefix_Given,
                          Stanza => To_Unbounded_String (S));
   end Stanza_Given;

   ------------------------------------
   --  Stanza_Type  --  Stanza_When  --
   ------------------------------------

   function Stanza_When  (S : in String) return Stanza_Type is begin
      return Stanza_Type'(Prefix => Prefix_When,
                          Stanza => To_Unbounded_String (S));
   end Stanza_When;

   ------------------------------------
   --  Stanza_Type  --  Stanza_Then  --
   ------------------------------------

   function Stanza_Then  (S : in String) return Stanza_Type is begin
      return Stanza_Type'(Prefix => Prefix_Then,
                          Stanza => To_Unbounded_String (S));
   end Stanza_Then;

end AdaSpec.Stanzas;
