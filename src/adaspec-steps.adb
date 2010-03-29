--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Fixed;

package body AdaSpec.Steps is

   -------------------------------------
   --  Stanza_Type  --  Stanza_Given  --
   -------------------------------------

   function Stanza_Given (S    : in String;
                          File : in String := "";
                          Line : in Positive := 1) return Stanza_Type is
   begin
      return Stanza_Type'(Prefix => Prefix_Given,
                          Stanza => To_Unbounded_String (S),
                          Pos    => (To_Unbounded_String (File), Line),
                          others => <>);
   end Stanza_Given;

   ------------------------------------
   --  Stanza_Type  --  Stanza_When  --
   ------------------------------------

   function Stanza_When  (S    : in String;
                          File : in String := "";
                          Line : in Positive := 1) return Stanza_Type is
   begin
      return Stanza_Type'(Prefix => Prefix_When,
                          Stanza => To_Unbounded_String (S),
                          Pos    => (To_Unbounded_String (File), Line),
                          others => <>);
   end Stanza_When;

   ------------------------------------
   --  Stanza_Type  --  Stanza_Then  --
   ------------------------------------

   function Stanza_Then  (S    : in String;
                          File : in String := "";
                          Line : in Positive := 1) return Stanza_Type is
   begin
      return Stanza_Type'(Prefix => Prefix_Then,
                          Stanza => To_Unbounded_String (S),
                          Pos    => (To_Unbounded_String (File), Line),
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

   ----------------------------------
   --  Stanza_Type  --  To_Regexp  --
   ----------------------------------

   function To_Regexp (S : in Stanza_Type) return String is
      use Ada.Strings.Fixed;
      Buffer    : Unbounded_String;
      Stanza    : constant String := To_String (S.Stanza);
      I         : Natural := Stanza'First;
      N         : Natural;
   begin
      case S.Prefix is
         when Prefix_Given => Append (Buffer, "@given ");
         when Prefix_When  => Append (Buffer, "@when ");
         when Prefix_Then  => Append (Buffer, "@then ");
      end case;
      Append (Buffer, "^");
      while I <= Stanza'Last loop
         case Stanza (I) is
            when '\' | '(' | ')' | '[' | ']' | '.' | '*' | '+' | '?' | '^' =>
               Append (Buffer, "\" & Stanza (I));
            when '"' =>
               N := Index (Stanza, """", I + 1);
               if N = 0 then
                  Append (Buffer, Stanza (I));
               else
                  Append (Buffer, """([^""]*)""");
                  I := N;
               end if;
            when others =>
               Append (Buffer, Stanza (I));
         end case;
         I := I + 1;
      end loop;
      Append (Buffer, "$");
      return To_String (Buffer);
   end To_Regexp;


   ---------------------------------
   --  Stanza_Type  --  Position  --
   ---------------------------------

   function Position (S : in Stanza_Type) return String is
   begin
      return To_String (S.Pos);
   end Position;

   ----------------------------
   --  Stanza_Type  --  "="  --
   ----------------------------

--    function "=" (Left, Right : in Stanza_Type) return Boolean is
--    begin
--       return   Left.Prefix = Right.Prefix and
--                Left.Stanza = Right.Stanza and
--                Left.Texts  = Right.Texts  and
--                Left.Pos    = Right.Pos;
--    end "=";

end AdaSpec.Steps;
