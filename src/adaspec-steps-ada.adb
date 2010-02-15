--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Text_IO;
with Util.IO;
with Util.Strings;
with AdaSpec;

use Ada.Strings.Unbounded;
use Ada.Directories;
use Ada.Text_IO;
use Util.IO;
use Util.Strings;
use AdaSpec;

package body AdaSpec.Steps.Ada is

   -----------------------
   --  Parse_Directory  --
   -----------------------

   procedure Parse_Directory (Steps     : in out Step_Vectors.Vector;
                              Directory : in     String)
   is
      use Step_Vectors;
      Search  : Search_Type;
      Element : Directory_Entry_Type;
      Step    : Ada_Step_File_Ptr;
   begin
      Start_Search (Search, Directory, "*.ads",
                    (Ordinary_File => True, others => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Element);
         Step := new Ada_Step_File_Type;
         Make (Step.all, Full_Name (Element));
         Parse (Step.all);
         Step_Vectors.Append (Steps, Step_File_Ptr (Step));
      end loop;
      End_Search (Search);
   end Parse_Directory;

   ------------
   --  Make  --
   ------------

   procedure Make (S         : out Ada_Step_File_Type;
                   File_Name : in String) is
   begin
      S := (File_Name => To_Unbounded_String (File_Name),
            Parsed    => False,
            Steps     => <>);
   end Make;

   --------------
   --  Parsed  --
   --------------

   function  Parsed (S : in Ada_Step_File_Type) return Boolean is
   begin
      return S.Parsed;
   end Parsed;

   -------------
   --  Parse  --
   -------------

   procedure Parse (S : in out Ada_Step_File_Type) is
      File     : File_Type;
      Line_S   : Unbounded_String;
      Idx_Next : Natural;
      Idx_Tk   : Natural;
      Idx      : Natural;
      Tokens   : constant String_List := (To_Unbounded_String ("@given"),
                                          To_Unbounded_String ("@when"),
                                          To_Unbounded_String ("@then"));
      Prefix   : Prefix_Type;
      Found    : Boolean;
      Pattern  : Unbounded_String;
   begin
      Open (File, In_File, To_String (S.File_Name));
      while not End_Of_File (File) loop
         --
         --  Read Line
         --
         Line_S := Get_Whole_Line (File);
         --
         --  Find Token
         --
         Find_Token (To_String (Line_S), Tokens, Idx_Next, Idx_Tk);
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
            Idx := Index_Non_Blank (Line_S, Idx_Next);
            if Idx /= 0 then
               Pattern := Unbounded_Slice (Line_S, Idx, Length (Line_S));
               S.Steps.Append (Step_Type'(
                  Prefix    => Prefix,
                  Pattern_R => Compile (To_String (Pattern)),
                  Pattern_S => Pattern));
            else
               --  TODO: use better reporting method
               Put_Line (Name (File) & ":" &
                         Natural'Image (Natural (Line (File))) & ":" &
                         Natural'Image (Idx_Next) & ": " &
                         "Warning: expecting argument");
            end if;
         end if;
      end loop;
      Close (File);
      S.Parsed := True;
   end Parse;

   ----------------
   --  Contains  --
   ----------------

   function  Contains  (S      : in Ada_Step_File_Type;
                        Prefix : in Prefix_Type;
                        Phrase : in String) return Boolean
   is
      Step  : Step_Type;
      Found : Boolean := False;
   begin
      --  Error if not parsed
      if not S.Parsed then
         raise Unparsed_Step;
      end if;

      --  Look for the phrase
      For_Loop : for i in S.Steps.First_Index .. S.Steps.Last_Index loop
         Step  := S.Steps.Element (i);
         if Step.Prefix = Prefix and Match (Phrase, Step.Pattern_R) then
            Found := True;
            exit For_Loop;
         end if;
      end loop For_Loop;

      --  No match
      return Found;
   end Contains;

end AdaSpec.Steps.Ada;
