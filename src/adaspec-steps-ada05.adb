--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.Regpat;
with Util.IO;
with Util.Strings;
with Util.Strings.Pool;
with AdaSpec;

use Ada.Strings.Unbounded;
use Ada.Directories;
use Ada.Text_IO;
use GNAT.Regpat;
use Util.IO;
use Util.Strings;
use Util.Strings.Pool;
use AdaSpec;

package body AdaSpec.Steps.Ada05 is

   -----------------------
   --  Parse_Directory  --
   -----------------------

   procedure Parse_Directory (Steps      : in out Steps_Type;
                              Logger     : in     Logger_Ptr;
                              Directory  : in     String;
                              Fill_Steps : in     Boolean := False)
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
         Step.Make  (Full_Name (Element), Fill_Steps);
         Step.Parse (Logger);
         Step_Vectors.Append (Steps, Step_File_Ptr (Step));
      end loop;
      End_Search (Search);
   end Parse_Directory;

   ------------
   --  Make  --
   ------------

   procedure Make (S          : out Ada_Step_File_Type;
                   File_Name  : in  String;
                   Fill_Steps : in  Boolean := False) is
   begin
      S := (File_Name  => To_Unbounded_String (File_Name),
            Parsed     => False,
            Fill_Steps => Fill_Steps,
            Steps      => <>);
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

   procedure Parse (S          : in out Ada_Step_File_Type;
                    Logger     : in     Logger_Ptr) is
      use Ada.Containers;
      use Step_Container;
      use Util.Strings.Vectors;
      File          : File_Type;
      Line_S        : Unbounded_String;
      Idx_Next      : Natural;
      Idx_Tk        : Natural;
      Idx, Idx2     : Natural;
      Tokens        : constant String_List
                    := (To_Unbounded_String ("@given"),
                        To_Unbounded_String ("@when"),
                        To_Unbounded_String ("@then"),
                        To_Unbounded_String ("@todo"),
                        To_Unbounded_String ("package "),
                        To_Unbounded_String ("procedure "));
      Prefix        : Prefix_Type;
      Found_Pkg     : Boolean;
      Found_Prc     : Boolean;
      Found_TODO    : Boolean;
      Found         : Boolean;
      Pattern       : Unbounded_String;
      Package_S     : Unbounded_String;
      Procedure_S   : Unbounded_String;
      Current_Steps : Step_Container.Vector;
      I             : Step_Container.Cursor;
      Current_Step  : Step_Type;
      Pending_Step  : Boolean := False;
      Char          : Character;
      Buffer        : Unbounded_String;
      Prc_Pool      : String_Pool;
      Added_Prc     : Util.Strings.Vectors.Vector;
      With_AdaSpecLib : Boolean := False;
      Use_AdaSpecLib  : Boolean := False;
   begin
      Open (File, In_File, To_String (S.File_Name));
      while not End_Of_File (File) loop
         --
         --  Read Line
         --
         Line_S := Get_Whole_Line (File);

         if Package_S = Null_Unbounded_String then
            Idx  := Index (Line_S, "with");
            Idx2 := Index (Line_S, "AdaSpecLib");
            if Idx > 0 and Idx < Idx2 then
               With_AdaSpecLib := True;
            end if;
            Idx  := Index (Line_S, "use");
            if Idx > 0 and Idx < Idx2 then
               Use_AdaSpecLib := True;
            end if;
         end if;

         --
         --  Find Token
         --
         Found_Pkg  := False;
         Found_Prc  := False;
         Found_TODO := False;
         Found      := False;

         Find_Token (To_String (Line_S), Tokens, Idx_Next, Idx_Tk);
         case Idx_Tk is
            when 1 =>   Prefix := Prefix_Given; Found := True;
            when 2 =>   Prefix := Prefix_When;  Found := True;
            when 3 =>   Prefix := Prefix_Then;  Found := True;
            when 4 =>   Found_TODO := True;
            when 5 =>   Found_Pkg  := True;
            when 6 =>   Found_Prc  := True;
            when others => null;
         end case;
         --
         --  Get Argument
         --
         if Found then
            Idx := Index_Non_Blank (Line_S, Idx_Next);
            if Idx /= 0 then
               Pattern := Unbounded_Slice (Line_S, Idx, Length (Line_S));
               --  if Pending_Step then
               --   --  TODO: use better reporting method
               --   Put_Line (Name (File) & ":" &
               --            Natural'Image (Natural (Line (File) - 1)) & ":" &
               --            Natural'Image (Idx_Next) & ": " &
               --            "Warning: expecting procedure for previous step");
               --  end if;
               Current_Step := Step_Type'(
                  Prefix    => Prefix,
                  --  TODO: free memory
                  Pattern_R => new Pattern_Matcher'(
                               Compile (To_String (Pattern))),
                  Pattern_S => Pattern,
                  others    => <>);
               Append (Current_Steps, Current_Step);
               Pending_Step := True;
            else
               --  TODO: use better reporting method
               Put_Line (Name (File) & ":" &
                         Natural'Image (Natural (Line (File) - 1)) & ":" &
                         Natural'Image (Idx_Next) & ": " &
                         "Warning: expecting argument");
            end if;
         elsif Found_Pkg then
            Idx       := Index_Non_Blank (Line_S, Idx_Next);
            Package_S := Null_Unbounded_String;
            Char := Element (Line_S, Idx);
            while
               Idx < Length (Line_S) and
               ((Char >= 'a' and Char <= 'z') or
                (Char >= 'A' and Char <= 'Z') or
                (Char >= '0' and Char <= '9') or
                Char = '.' or Char = '_')
            loop
               Append (Package_S, Char);
               Idx  := Idx + 1;
               Char := Element (Line_S, Idx);
            end loop;
--             Put_Line ("Package " & To_String (Package_S));
         elsif Found_Prc and Pending_Step then
            Idx         := Index_Non_Blank (Line_S, Idx_Next);
            Procedure_S := Null_Unbounded_String;
            Char := Element (Line_S, Idx);
            while
               Idx < Length (Line_S) and
               ((Char >= 'a' and Char <= 'z') or
                (Char >= 'A' and Char <= 'Z') or
                (Char >= '0' and Char <= '9') or
                Char = '_')
            loop
               Append (Procedure_S, Char);
               Idx  := Idx + 1;
               Char := Element (Line_S, Idx);
            end loop;
--             Put_Line ("Procedure " & To_String (Procedure_S));
            Add_Pool (Prc_Pool, To_String (Procedure_S));
            I := First (Current_Steps);
            while Has_Element (I) loop
               Current_Step := Element (I);
               Current_Step.Proc_Name := Package_S & '.' & Procedure_S;
               S.Steps.Append (Current_Step);
               Next (I);
            end loop;
            Clear (Current_Steps);
            Pending_Step := False;
         elsif Found_TODO then
            if S.Fill_Steps and Length (Current_Steps) > 0 then
               --  Copy indentation
               Idx := Index_Non_Blank (Line_S);
               if Idx /= 0 then
                  Append (Buffer, Slice (Line_S, 1, Idx - 1));
               end if;
               --  Create procedure specification
               Append (Buffer, "procedure ");
               if Length (Current_Steps) = 1 then
                  Current_Step := First_Element (Current_Steps);
                  Procedure_S := Null_Unbounded_String;
                  case Current_Step.Prefix is
                     when Prefix_Given => Append (Procedure_S, "Given_");
                     when Prefix_When  => Append (Procedure_S, "When_");
                     when Prefix_Then  => Append (Procedure_S, "Then_");
                  end case;
                  Append (Procedure_S,
                     To_Identifier (To_String (Current_Step.Pattern_S)));
               else
                  Procedure_S := To_Unbounded_String ("Mixed_Step");
               end if;
               Get_Unique_String (Prc_Pool, To_String (Procedure_S),
                                    Procedure_S);
               Append (Added_Prc, Procedure_S);
               Append (Buffer, Procedure_S);
               Append (Buffer, " (Args : in out Arg_Type);" & ASCII.LF);
            end if;
            I := First (Current_Steps);
            while Has_Element (I) loop
               Current_Step := Element (I);
               Current_Step.Proc_Name := Null_Unbounded_String;
               S.Steps.Append (Current_Step);
               Next (I);
            end loop;
            Clear (Current_Steps);
            Pending_Step := False;
         end if;
         if S.Fill_Steps and not Found_TODO then
            Append (Buffer, Line_S & ASCII.LF);
         end if;
      end loop;
      if Pending_Step then
         Logger.Put_Line ("Warning: step definition with no matching " &
                          "procedure in:");
         Logger.Put_Line ("  " & Name (File) & " line" &
                          Natural (Line (File) - 1)'Img);
      end if;
      Close (File);
      if S.Fill_Steps then
         if not Use_AdaSpecLib then
            Buffer := "use  AdaSpecLib;" & ASCII.LF & Buffer;
         end if;
         if not With_AdaSpecLib then
            Buffer := "with AdaSpecLib;" & ASCII.LF & Buffer;
         end if;
         Set_File (To_String (S.File_Name), To_String (Buffer));
      end if;
      S.Parsed := True;
   end Parse;

   ------------
   --  Find  --
   ------------

   procedure Find      (S       : in     Ada_Step_File_Type;
                        Stanza  : in     Stanza_Type;
                        Proc    : out    Unbounded_String;
                        Matches : out    Match_Vectors.Vector;
                        Found   : out    Boolean)
   is
      use Match_Vectors;
      Step     : Step_Type;
      Matches2 : Match_Vectors.Vector;
   begin
      Found := False;

      --  Error if not parsed
      if not S.Parsed then
         raise Unparsed_Step;
      end if;

      --  Look for the phrase
      for i in S.Steps.First_Index .. S.Steps.Last_Index loop
         Step  := S.Steps.Element (i);
         if Step.Prefix = Stanza.Prefix then
            declare
               Matched  : Match_Array (0 .. Paren_Count (Step.Pattern_R.all));
            begin
--                Put_Line ("AdaSpec.Steps.Ada.Find: Match """ &
--                          To_String (Stanza.Stanza) & """ against |" &
--                          To_String (Step.Pattern_S) & "|");
               Match (Step.Pattern_R.all, To_String (Stanza.Stanza), Matched);
               if Matched (0) /= No_Match then
                  if Found then
                     raise Ambiguous_Match;
                  end if;
--                   Put_Line ("Matched (0) = " &
--                             Slice (Stanza.Stanza,
--                                    Matched (0).First,
--                                    Matched (0).Last));
                  Proc  := Step.Proc_Name;
                  Found := True;
                  for J in Matched'First + 1 .. Matched'Last loop
--                      Put_Line ("Matched (" & J'Img & ") = " &
--                               Slice (Stanza.Stanza,
--                                     Matched (J).First,
--                                     Matched (J).Last));
                     if Matched (J) /= No_Match then
                        Append (Matches2, Match_Location'(Matched (J).First,
                                                          Matched (J).Last));
                     end if;
                  end loop;  --  GCOV_IGNORE
                  Matches := Matches2;
--                else
--                   Put_Line ("Matched (0) = No_Match");
               end if;
            end;  --  GCOV_IGNORE
--          else
--             Put_Line ("AdaSpec.Steps.Ada.Find: Don't Match " &
--                       Stanza.Prefix'Img & " """ &
--                       To_String (Stanza.Stanza) & """ against " &
--                       Step.Prefix'Img & " |" &
--                       To_String (Step.Pattern_S) & "|");
         end if;
      end loop;

      --  No match
--       Put_Line ("AdaSpec.Steps.Ada.Find: Not found");
   end Find;


   ----------------
   --  Finalize  --
   ----------------

   overriding procedure Finalize  (S : in out Ada_Step_File_Type) is
      use Step_Container;
      I : Step_Container.Cursor := First (S.Steps);
      E : Step_Type;
   begin
      while Has_Element (I) loop
         E := Element (I);
         Free (E.Pattern_R);
         Next (I);
      end loop;
   end Finalize;



end AdaSpec.Steps.Ada05;
