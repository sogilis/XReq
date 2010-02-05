--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Util.IO;
with Util.Strings;

use Ada.Strings.Unbounded;
use Ada.Text_IO;
use Util.IO;
use Util.Strings;

package body AdaSpec.Steps.Ada is

   procedure Make (S         : in out Ada_Step_File_Type;
                   File_Name : in String) is
   begin
      S.File_Name := To_Unbounded_String (File_Name);
      S.Parsed    := False;
   end Make;

   function  Parsed (S : in Ada_Step_File_Type) return Boolean is
   begin
      return S.Parsed;
   end Parsed;

   procedure Parse (S : in out Ada_Step_File_Type) is
      File     : File_Type;
      Line     : Unbounded_String;
      Idx_Next : Natural;
      Idx_Tk   : Natural;
      Idx      : Natural;
      Tokens   : constant String_List := (To_Unbounded_String ("@given"),
                                          To_Unbounded_String ("@when"),
                                          To_Unbounded_String ("@then"));
      Prefix   : Prefix_Type;
      Found    : Boolean;
      Pattern  : Regexp;
   begin
      Open (File, In_File, To_String (S.File_Name));
      while not End_Of_File (File) loop
         --
         --  Read Line
         --
         Line := Get_Whole_Line (File);
         --
         --  Find Token
         --
         Find_Token (To_String (Line), Tokens, Idx_Next, Idx_Tk);
         case Idx_Tk is
            when 1 =>   Prefix := Prefix_Given; Found := True;
            when 2 =>   Prefix := Prefix_When;  Found := True;
            when 3 =>   Prefix := Prefix_Then;  Found := True;
            when others =>                      Found := False;
         end case;
         --
         --  Get Argument
         --
         if Found then
            Idx := Index_Non_Blank (Line, Idx_Next);
            Pattern := Compile (Slice (Line, Idx, Length (Line)));
            S.Steps.Append (Step_Type'(
               Prefix  => Prefix,
               Pattern => Pattern,
               Pat_S   => Unbounded_Slice (Line, Idx, Length (Line))));
         end if;
      end loop;
      Close (File);
      S.Parsed := True;
   end Parse;

   function  Contains  (S      : in Ada_Step_File_Type;
                        Prefix : in Prefix_Type;
                        Phrase : in String) return Boolean
   is
      Step  : Step_Type;
   begin
      --  Error if not parsed
      if not S.Parsed then
         raise Unparsed_Step;
      end if;

      --  Look for the phrase
      for i in S.Steps.First_Index .. S.Steps.Last_Index loop
         Step  := S.Steps.Element (i);
         if Step.Prefix = Prefix and Match (Phrase, Step.Pattern) then
            return True;
         end if;
      end loop;

      --  No match
      return False;
   end Contains;

end AdaSpec.Steps.Ada;
