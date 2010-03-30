--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Fixed;

package body AdaSpecLib.Generic_Steps is

   -------------------------------
   --  Step_Type  --  New_Step  --
   -------------------------------

   function  New_Step    (Kind     : in Step_Kind;
                          Stanza   : in String;
                          Position : in Position_Type) return Step_Type is
   begin
      return Step_Type'(Prefix => Kind,
                        Stanza => To_Unbounded_String (Stanza),
                        Pos    => Position,
                        others => <>);
   end New_Step;

   -------------------------------------
   --  Stanza_Type  --  Stanza_Given  --
   -------------------------------------

   function Stanza_Given (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Step_Type is
   begin
      return New_Step (Step_Given, S, Position (File, Line));
   end Stanza_Given;

   ------------------------------------
   --  Stanza_Type  --  Stanza_When  --
   ------------------------------------

   function Stanza_When  (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Step_Type is
   begin
      return New_Step (Step_When, S, Position (File, Line));
   end Stanza_When;

   ------------------------------------
   --  Stanza_Type  --  Stanza_Then  --
   ------------------------------------

   function Stanza_Then  (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Step_Type is
   begin
      return New_Step (Step_Then, S, Position (File, Line));
   end Stanza_Then;


   ----------------------------------
   --  Stanza_Type  --  To_String  --
   ----------------------------------

   function To_String (S : in Step_Type;
                       K : in Step_All_Kind := Step_Null) return String is
      Buffer : Unbounded_String;
   begin
      if S.Prefix = K then
         Append (Buffer, "And ");
      else
         case S.Prefix is
            when Step_Given => Append (Buffer, "Given ");
            when Step_When  => Append (Buffer, "When ");
            when Step_Then  => Append (Buffer, "Then ");
         end case;
      end if;
      Append (Buffer, S.Stanza);
      return To_String (Buffer);
   end To_String;

   ----------------------------------
   --  Stanza_Type  --  To_Regexp  --
   ----------------------------------

   function To_Regexp (S : in Step_Type) return String is
      use Ada.Strings.Fixed;
      Buffer    : Unbounded_String;
      Stanza    : constant String := To_String (S.Stanza);
      I         : Natural := Stanza'First;
      N         : Natural;
   begin
      case S.Prefix is
         when Step_Given => Append (Buffer, "@given ");
         when Step_When  => Append (Buffer, "@when ");
         when Step_Then  => Append (Buffer, "@then ");
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

   ----------------------------------
   --  Stanza_Type  --  Arg_First  --
   ----------------------------------

   function Arg_First (S : in Step_Type) return Natural is
      pragma Unreferenced (S);
   begin
      return 0;
   end Arg_First;

   ---------------------------------
   --  Stanza_Type  --  Arg_Last  --
   ---------------------------------

   function Arg_Last  (S : in Step_Type) return Integer is
      use Argument_Vectors;
      use Ada.Containers;
   begin
      return Integer (Length (S.Args)) - 1;
   end Arg_Last;

   ---------------------------------
   --  Stanza_Type  --  Arg_Last  --
   ---------------------------------

   function Arg_Element (S : in Step_Type;
                         I : in Natural)   return Argument_Type is
      use Argument_Vectors;
   begin
      return Element (S.Args, I);
   end Arg_Element;

   ---------------------------------
   --  Stanza_Type  --  Position  --
   ---------------------------------

   function Position (S : in Step_Type) return Position_Type is
   begin
      return S.Pos;
   end Position;

   -------------------------------
   --  Stanza_Type  --  Stanza  --
   -------------------------------

   function Stanza    (S : in Step_Type) return String is
   begin
      return To_String (S.Stanza);
   end Stanza;

   -----------------------------
   --  Stanza_Type  --  Kind  --
   -----------------------------

   function Kind      (S : in Step_Type) return Step_Kind is
   begin
      return S.Prefix;
   end Kind;
   -------------------------------------
   --  Stanza_Type  --  Set_Position  --
   -------------------------------------

   procedure Set_Position (S      : in out Step_Type;
                           Pos    : in     Position_Type) is
   begin
      S.Pos := Pos;
   end Set_Position;

   -----------------------------------
   --  Stanza_Type  --  Set_Stanza  --
   -----------------------------------

   procedure Set_Stanza   (S      : in out Step_Type;
                           Stanza : in     String) is
   begin
      S.Stanza := To_Unbounded_String (Stanza);
   end Set_Stanza;

   ---------------------------------
   --  Stanza_Type  --  Set_Kind  --
   ---------------------------------

   procedure Set_Kind     (S      : in out Step_Type;
                           Kind   : in     Step_Kind) is
   begin
      S.Prefix := Kind;
   end Set_Kind;

   -----------------------------------
   --  Stanza_Type  --  Arg_Append  --
   -----------------------------------

   procedure Arg_Append   (S      : in out Step_Type;
                           E      : in     Argument_Type) is
      use Argument_Vectors;
   begin
      Append (S.Args, E);
   end Arg_Append;

   -----------------------------
   --  Step_Type  --  Equals  --
   -----------------------------

   function Equals (Left, Right : in Step_Type) return Boolean is
      L : constant access constant Step_Type'Class := Left'Access;
      R : constant access constant Step_Type'Class := Right'Access;
   begin
      return L.all = R.all;
   end Equals;

end AdaSpecLib.Generic_Steps;
