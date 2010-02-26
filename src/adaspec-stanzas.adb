--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Stanzas is

   -------------------------------------
   --  Stanza_Type  --  Stanza_Given  --
   -------------------------------------

   function Stanza_Given (S : in String) return Stanza_Type is begin
      return Stanza_Type'(Prefix => Prefix_Given,
                          Stanza => To_Unbounded_String (S),
                          others => <>);
   end Stanza_Given;

   ------------------------------------
   --  Stanza_Type  --  Stanza_When  --
   ------------------------------------

   function Stanza_When  (S : in String) return Stanza_Type is begin
      return Stanza_Type'(Prefix => Prefix_When,
                          Stanza => To_Unbounded_String (S),
                          others => <>);
   end Stanza_When;

   ------------------------------------
   --  Stanza_Type  --  Stanza_Then  --
   ------------------------------------

   function Stanza_Then  (S : in String) return Stanza_Type is begin
      return Stanza_Type'(Prefix => Prefix_Then,
                          Stanza => To_Unbounded_String (S),
                          others => <>);
   end Stanza_Then;


   ----------------------------------
   --  Stanza_Type  --  To_String  --
   ----------------------------------

   function To_String (S : in Stanza_Type) return String is
      Buffer : Unbounded_String;
   begin
      case S.Prefix is
         when Prefix_Given => Append (Buffer, "Given ");
         when Prefix_When  => Append (Buffer, "When ");
         when Prefix_Then  => Append (Buffer, "Then ");
--          when others       => Append (Buffer, "<?> ");
      end case;
      Append (Buffer, S.Stanza);
      return To_String (Buffer);
   end To_String;


end AdaSpec.Stanzas;
