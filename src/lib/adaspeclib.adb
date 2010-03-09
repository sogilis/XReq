--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpecLib is

   --------------
   --  Assert  --
   --------------

   procedure Assert (Cmp : in Boolean; Reason : in String := "") is
   begin
      if not Cmp then
         if Reason /= "" then
            raise Error with Reason;
         else
            raise Error with "Assertion failed";
         end if;
      end if;
   end Assert;

   --------------
   --  Equals  --
   --------------

   procedure Equals (T1, T2 : in String; Reason : in String := "") is
   begin
      if T1 /= T2 then
         if Reason /= "" then
            raise Error with Reason & ASCII.LF &
               "Expected two strings to be equals:" & ASCII.LF &
               T1 & ASCII.LF & "--  is not the same as  --" & ASCII.LF &
               T2 & ASCII.LF & "--";
         else
            raise Error with
               "Expected two strings to be equals:" & ASCII.LF &
               T1 & ASCII.LF & "--  is not the same as  --" & ASCII.LF &
               T2 & ASCII.LF & "--";
         end if;
      end if;
   end Equals;

   --------------------------
   --  Arg_Type  --  Make  --
   --------------------------

   procedure Make        (Self   : out    Arg_Type;
                          Stanza : in     String) is
   begin
      Self := (Stanza => To_Unbounded_String (Stanza),
               others => <>);
   end Make;

   ---------------------------
   --  Arg_Type  --  Match  --
   ---------------------------

   function  Match       (Self   : in     Arg_Type;
                          N      : in     Natural)  return String
   is
      First, Last : Natural;
   begin
      Self.Match (N, First, Last);
      return Slice (Self.Stanza, First, Last);
   end Match;

   ---------------------------
   --  Arg_Type  --  Match  --
   ---------------------------

   procedure Match       (Self   : in     Arg_Type;
                          N      : in     Natural;
                          First  : out    Natural;
                          Last   : out    Natural)
   is
      use Match_Vectors;
      Match : Match_Type;
   begin
      if N = 0 then
         First := 1;
         Last  := Length (Self.Stanza);
      else
         Match := Element (Self.Matches, N);
         First := Match.First;
         Last  := Match.Last;
      end if;
   end Match;

   ----------------------------
   --  Arg_Type  --  Stanza  --
   ----------------------------

   function  Stanza      (Self   : in     Arg_Type) return String is
   begin
      return To_String (Self.Stanza);
   end Stanza;

   ---------------------------------
   --  Arg_Type  --  First_Match  --
   ---------------------------------

   function  First_Match (Self   : in     Arg_Type) return Natural is
      pragma Unreferenced (Self);
   begin
      return 0;
   end First_Match;

   --------------------------------
   --  Arg_Type  --  Last_Match  --
   --------------------------------

   function  Last_Match  (Self   : in     Arg_Type) return Natural is
      use Match_Vectors;
   begin
      return Natural (Length (Self.Matches));
   end Last_Match;

   -------------------------------
   --  Arg_Type  --  Add_Match  --
   -------------------------------

   procedure Add_Match   (Self   : in out Arg_Type;
                          First  : in     Natural;
                          Last   : in     Natural)
   is
      use Match_Vectors;
   begin
      Append (Self.Matches, Match_Type'(First, Last));
   end Add_Match;

   --------------------------
   --  Arg_Type  --  Text  --
   --------------------------

   function  Text        (Self   : in     Arg_Type;
                          N      : in     Natural := 0) return String
   is
      use String_Vectors;
   begin
      return To_String (Element (Self.Texts, N));
   end Text;

   ------------------------------
   --  Arg_Type  --  Add_Text  --
   ------------------------------

   procedure Add_Text    (Self   : in out Arg_Type;
                          Text   : in     String)
   is
      use String_Vectors;
   begin
      Append (Self.Texts, To_Unbounded_String (Text));
   end Add_Text;

   --------------------------------
   --  Arg_Type  --  First_Text  --
   --------------------------------

   function  First_Text  (Self   : in     Arg_Type) return Natural is
      pragma Unreferenced (Self);
   begin
      return 0;
   end First_Text;

   -------------------------------
   --  Arg_Type  --  Last_Text  --
   -------------------------------

   function  Last_Text   (Self   : in     Arg_Type) return Integer is
      use String_Vectors;
   begin
      return Natural (Length (Self.Texts)) - 1;
   end Last_Text;

   ------------------------------
   --  Arg_Type  --  Num_Text  --
   ------------------------------

   function  Num_Text    (Self   : in     Arg_Type) return Natural is
      use String_Vectors;
   begin
      return Natural (Length (Self.Texts));
   end Num_Text;



end AdaSpecLib;