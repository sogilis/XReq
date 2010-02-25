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

end AdaSpecLib;