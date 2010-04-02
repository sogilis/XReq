--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.Regpat;
with Util.IO;
with Util.Strings;
with AdaSpec;

use Ada.Strings.Unbounded;
use Ada.Directories;
use Ada.Text_IO;
use GNAT.Regpat;
use Util.IO;
use Util.Strings;
use AdaSpec;

package body AdaSpec.Step_Definitions.Ada05 is

   function Procedure_Spec (Proc_Name : in String) return String;
   function Procedure_Body (Proc_Name : in String) return String;

   procedure Write_Adb_File (File_Name      : in String;
                             Package_Name   : in String;
                             Procedures     : in String_Vector;
                             Logger         : in Logger_Ptr);
   procedure Write_Ads_File (File_Name      : in String;
                             Package_Name   : in String;
                             Procedures     : in String_Vector;
                             Tags           : in String_Vector;
                             Logger         : in Logger_Ptr);

   procedure Unique_Procedure_Name (Prc_Pool : in out String_Pool;
                                    Result   : out    Unbounded_String;
                                    Name     : in     String := "Mixed_Step");
   procedure Unique_Procedure_Name (Prc_Pool : in out String_Pool;
                                    Result   : out    Unbounded_String;
                                    Prefix   : in     Step_Kind;
                                    Pattern  : in     String);

   procedure Parse (File_Name  : in     String;
                    Fill_Steps : in     Boolean;
                    Steps      : in out Step_Container.Vector;
                    Prc_Pool   : in out String_Pool;
                    Logger     : in     Logger_Ptr);

   procedure Finalize (Steps : in out Step_Container.Vector);

   -----------------------
   --  Parse_Directory  --
   -----------------------

   procedure Parse_Directory (Steps      : in out Step_Definitions_Type;
                              Logger     : in     Logger_Ptr;
                              Directory  : in     String;
                              Fill_Steps : in     Boolean := False)
   is
      use Step_Definition_Vectors;
      Search  : Search_Type;
      Element : Directory_Entry_Type;
      Step    : Ada_Step_File_Ptr;
   begin
      Start_Search (Search, Directory, "*.ads",
                    (Ordinary_File => True, others => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Element);
         Step := new Ada_Step_File_Type;
         Step.Make  (Compose (Directory, Simple_Name (Element)), Fill_Steps);
         Step.Parse (Logger);
         Step_Definition_Vectors.Append (Steps, Step_File_Ptr (Step));
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
            others     => <>);
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
                    Logger     : in     Logger_Ptr)
   is
      use String_Sets;
   begin
      Parse (File_Name  => To_String (S.File_Name),
             Fill_Steps => S.Fill_Steps,
             Steps      => S.Steps,
             Prc_Pool   => S.Procedures,
             Logger     => Logger);
      S.Parsed := True;
   end Parse;

   -------------
   --  Parse  --
   -------------

   procedure Parse (File_Name  : in     String;
                    Fill_Steps : in     Boolean;
                    Steps      : in out Step_Container.Vector;
                    Prc_Pool   : in out String_Pool;
                    Logger     : in     Logger_Ptr)
   is
      use Ada.Containers;
      use Ada.Strings.Fixed;
      use Ada.Strings;
      use Step_Container;
      use String_Vectors;
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
      Prefix        : Step_Kind;
      Found_Pkg     : Boolean;
      Found_Prc     : Boolean;
      Found_TODO    : Boolean;
      Found         : Boolean;
      Pattern       : Unbounded_String;
      Package_S     : Unbounded_String;
      Procedure_S   : Unbounded_String;
      Current_Steps : Step_Container.Vector;
      I             : Step_Container.Cursor;
      Current_Step  : Step_Definition_Type;
      Pending_Step  : Boolean := False;
      Char          : Character;
      Buffer        : Unbounded_String; -- The .ads file with @todo replaced
      Added_Prc     : String_Vectors.Vector;
      With_AdaSpecLib : Boolean := False;
      Use_AdaSpecLib  : Boolean := False;
      Steps_Ads_File  : constant String := File_Name;
      Steps_Adb_File  : constant String :=
         Steps_Ads_File (Steps_Ads_File'First .. Steps_Ads_File'Last - 3) &
         "adb";
      Position      : Position_Type;
   begin
      Finalize (Steps);
      Position.File := To_Unbounded_String (File_Name);
      Open (File, In_File, Steps_Ads_File);
      while not End_Of_File (File) loop
         --
         --  Read Line
         --
         Line_S := Get_Whole_Line (File);
         Position.Line := Position.Line + 1;

         --
         --  Check with/use AdaSpecLib.General
         --
         if Package_S = Null_Unbounded_String then
            Idx  := Index (Line_S, "with");
            Idx2 := Index (Line_S, "AdaSpecLib.General");
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
            when 1 =>   Prefix := Step_Given; Found := True;
            when 2 =>   Prefix := Step_When;  Found := True;
            when 3 =>   Prefix := Step_Then;  Found := True;
            when 4 =>   Found_TODO := True;
            when 5 =>   Found_Pkg  := True;
            when 6 =>   Found_Prc  := True;
            when others => null;
         end case;

         --
         --  Found @given, @when or @then - Get Argument
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
               Current_Step := Step_Definition_Type'(
                  Prefix    => Prefix,
                  --  TODO: free memory
                  Pattern_R => new Pattern_Matcher'(
                               Compile (To_String (Pattern))),
                  Pattern_S => Pattern,
                  Position  => Position,
                  others    => <>);
               Append (Current_Steps, Current_Step);
               Pending_Step := True;
            else
               --  TODO: use better reporting method
               Logger.Put_Line (String'(
                  "WARNING: Expecting argument in " &
                  To_String (Position) & ":" &
                  Trim (Idx_Next'Img, Left)));
            end if;

         --
         --  Found "package" - Get its name
         --
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

         --
         --  Found "procedure" - Get its name
         --
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
               Steps.Append (Current_Step);
               Next (I);
            end loop;
            Clear (Current_Steps);
            Pending_Step := False;

         --
         --  Found @todo
         --
         elsif Found_TODO then
            if Fill_Steps and Length (Current_Steps) > 0 then
               --  Copy indentation
               Idx := Index_Non_Blank (Line_S);
               if Idx /= 0 then
                  Append (Buffer, Slice (Line_S, 1, Idx - 1));
               end if;
               --  Create procedure specification
               if Length (Current_Steps) = 1 then
                  Current_Step := First_Element (Current_Steps);
                  Unique_Procedure_Name (Prc_Pool,
                                         Procedure_S,
                                         Current_Step.Prefix,
                                         To_String (Current_Step.Pattern_S));
               else
                  Unique_Procedure_Name (Prc_Pool, Procedure_S);
               end if;
               Append (Added_Prc, Procedure_S);
               Append (Buffer,
                       Procedure_Spec (To_String (Procedure_S)) & ASCII.LF);
            end if;
            I := First (Current_Steps);
            while Has_Element (I) loop
               Current_Step := Element (I);
               Current_Step.Proc_Name := Null_Unbounded_String;
               Steps.Append (Current_Step);
               Next (I);
            end loop;
            Clear (Current_Steps);
            Pending_Step := False;
         end if;
         if Fill_Steps and not Found_TODO then
            Append (Buffer, Line_S & ASCII.LF);
         end if;
      end loop;
      if Pending_Step then
         Logger.Put_Line ("Warning: step definition with no matching " &
                          "procedure in:");
         Logger.Put_Line ("  " & Steps_Ads_File & " line" &
                          Natural (Line (File) - 1)'Img);
      end if;
      Close (File);
      ------------------
      --  Fill Steps  --
      ------------------
      if Fill_Steps then
         if not Is_Empty (Added_Prc) then
            if not Use_AdaSpecLib then
               Buffer := "use  AdaSpecLib.General;" & ASCII.LF & Buffer;
            end if;
            if not With_AdaSpecLib then
               Buffer := "with AdaSpecLib.General;" & ASCII.LF & Buffer;
            end if;
         end if;
         Logger.Put_Line ("Update: " & Steps_Ads_File);
         Set_File (Steps_Ads_File, To_String (Buffer));
         Write_Adb_File (Steps_Adb_File, To_String (Package_S),
                         Added_Prc, Logger);
      end if;
   end Parse;

   ----------------------
   --  Procedure_Spec  --
   ----------------------

   function Procedure_Spec (Proc_Name : in String) return String is
   begin
      return "procedure " & Proc_Name & " (Args : in out Arg_Type);";
   end Procedure_Spec;

   ----------------------
   --  Procedure_Body  --
   ----------------------

   function Procedure_Body (Proc_Name : in String) return String is
      LF : constant String := "" & ASCII.LF;
   begin
      return
        "   procedure " & Proc_Name & " (Args : in out Arg_Type) is" & LF &
        "      Not_Yet_Implemented : exception;"                     & LF &
        "   begin"                                                   & LF &
        "      raise Not_Yet_Implemented"                            & LF &
        "         with ""Procedure "" & " & Ada_String (Proc_Name) &
                                    " & "" not implemented"";"       & LF &
        "   end " & Proc_Name & ";"                                  & LF;
   end Procedure_Body;

   ----------------------
   --  Write_Adb_File  --
   ----------------------

   procedure Write_Adb_File (File_Name      : in String;
                             Package_Name   : in String;
                             Procedures     : in String_Vector;
                             Logger         : in Logger_Ptr)
   is
      use String_Vectors;
      File   : File_Type;
      I      : String_Vectors.Cursor;
      Buffer : Unbounded_String;
      Line   : Unbounded_String;
   begin
      if not Exists (File_Name) then
         Logger.Put_Line ("Generate: " & File_Name);
         Create (File, Out_File, File_Name);
         Put_Line (File, "package body " & Package_Name & " is");
         New_Line (File);
         I := First (Procedures);
         while Has_Element (I) loop
            Put_Line (File, Procedure_Body (To_String (Element (I))));
            Next (I);
         end loop;
         Put_Line (File, "end " & Package_Name & ";");
         Close (File);
      else
         Logger.Put_Line ("Update: " & File_Name);
         Open (File, In_File, File_Name);
         while not End_Of_File (File) loop
            Line := Get_Whole_Line (File);
            if Index (Line, "end") = 1 then
               I := First (Procedures);
               while Has_Element (I) loop
                  Append (Buffer,
                          Procedure_Body (To_String (Element (I))) & ASCII.LF);
                  Next (I);
               end loop;
            end if;
            Append (Buffer, Line & ASCII.LF);
         end loop;
         Close (File);
         Set_File (File_Name, To_String (Buffer));
      end if;
   end Write_Adb_File;

   ----------------------
   --  Write_Ads_File  --
   ----------------------

   procedure Write_Ads_File (File_Name      : in String;
                             Package_Name   : in String;
                             Procedures     : in String_Vector;
                             Tags           : in String_Vector;
                             Logger         : in Logger_Ptr)
   is
      use String_Vectors;
      File   : File_Type;
      I      : String_Vectors.Cursor;
      J      : String_Vectors.Cursor;
      Buffer : Unbounded_String;
      Line   : Unbounded_String;
      Idx    : Integer;
      Idx2   : Integer;
      With_AdaSpecLib : Boolean := False;
      Use_AdaSpecLib  : Boolean := False;
   begin
      if not Exists (File_Name) then
         Logger.Put_Line ("Generate: " & File_Name);
         Create (File, Out_File, File_Name);
         Put_Line (File, "with AdaSpecLib.General;");
         Put_Line (File, "use  AdaSpecLib.General;");
         Put_Line (File, "package " & Package_Name & " is");
         New_Line (File);
         I := First (Procedures);
         J := First (Tags);
         while Has_Element (I) loop
            Put_Line (File, "   --  " & To_String (Element (J)));
            Put_Line (File, "   " & Procedure_Spec (To_String (Element (I))));
            New_Line (File);
            Next (I);
            Next (J);
         end loop;
         Put_Line (File, "end " & Package_Name & ";");
         Close (File);
      else
         Logger.Put_Line ("Update: " & File_Name);
         Open (File, In_File, File_Name);
         while not End_Of_File (File) loop
            Line := Get_Whole_Line (File);
            Idx  := Index (Line, "with");
            Idx2 := Index (Line, "AdaSpecLib.General");
            if Idx > 0 and Idx < Idx2 then
               With_AdaSpecLib := True;
            end if;
            Idx  := Index (Line, "use");
            if Idx > 0 and Idx < Idx2 then
               Use_AdaSpecLib := True;
            end if;
            if Index (Line, "end") = 1 then
               I := First (Procedures);
               J := First (Tags);
               while Has_Element (I) loop
                  Append (Buffer,
                          "   --  " & To_String (Element (J)) & ASCII.LF &
                          "   " & Procedure_Spec (To_String (Element (I))) &
                          ASCII.LF & ASCII.LF);
                  Next (I);
                  Next (J);
               end loop;
            end if;
            Append (Buffer, Line & ASCII.LF);
         end loop;
         Close (File);
         if not Use_AdaSpecLib then
            Buffer := "use  AdaSpecLib.General;" & ASCII.LF & Buffer;
         end if;
         if not With_AdaSpecLib then
            Buffer := "with AdaSpecLib.General;" & ASCII.LF & Buffer;
         end if;
         Set_File (File_Name, To_String (Buffer));
      end if;
   end Write_Ads_File;

   -----------------------------
   --  Unique_Procedure_Name  --
   -----------------------------

   procedure Unique_Procedure_Name (Prc_Pool : in out String_Pool;
                                    Result   : out    Unbounded_String;
                                    Name     : in     String := "Mixed_Step")
   is
   begin
      Get_Unique_String (Prc_Pool, Name, Result);
   end Unique_Procedure_Name;

   procedure Unique_Procedure_Name (Prc_Pool : in out String_Pool;
                                    Result   : out    Unbounded_String;
                                    Prefix   : in     Step_Kind;
                                    Pattern  : in     String)
   is
      Proc_Name : Unbounded_String;
   begin
      case Prefix is
         when Step_Given => Append (Proc_Name, "Given_");
         when Step_When  => Append (Proc_Name, "When_");
         when Step_Then  => Append (Proc_Name, "Then_");
      end case;
      Append (Proc_Name, To_Identifier (Pattern));
      Unique_Procedure_Name (Prc_Pool, Result, To_String (Proc_Name));
   end Unique_Procedure_Name;

   -----------------
   --  Add_Steps  --
   -----------------

   procedure Add_Steps       (Steps      : in out Step_Definitions_Type;
                              New_Steps  : in     String_Set;
                              Step_Pkg   : in     String;
                              Directory  : in     String;
                              Logger     : in     Logger_Ptr)
   is
      use Step_Definition_Vectors;
      use String_Sets;
      use String_Vectors;

      Pkg_Id        : constant String  := To_Package_File_Id (Step_Pkg);
      File_Name_Ads : constant String  := Compose (Directory, Pkg_Id & ".ads");
      File_Name_Adb : constant String  := Compose (Directory, Pkg_Id & ".adb");

      I : Step_Definition_Vectors.Cursor;
      J : String_Sets.Cursor;
      S : Ada_Step_File_Ptr := null;

      Tag        : Unbounded_String;
      Prc_Name   : Unbounded_String;
      Procedures : String_Vector;
      Tags       : String_Vector;
   begin

      I := First (Steps);
      while Has_Element (I) and S = null loop
         S := Ada_Step_File_Ptr (Element (I));
         if S /= null and then S.File_Name /= File_Name_Ads then
            --  This line is difficult to cover for certain as it depends on
            --  the order the step packages are read. If the step package we
            --  are modifying is the first, then this line is never executed.
            S := null;  --  GCOV_IGNORE
         end if;
         Next (I);
      end loop;

      if S = null then
         S := new Ada_Step_File_Type;
         S.Make  (File_Name_Ads, Fill_Steps => False);
         if Exists (File_Name_Ads) then
            --  GCOV_IGNORE_BEGIN
            --  Should not never happen, unless the parsing of the step
            --  directory missed a file, or unless a new file was created
            --  in between
            S.Parse (Logger);
            --  GCOV_IGNORE_END
         end if;
         Append  (Steps, Step_File_Ptr (S));
         I := Last (Steps);
         Logger.Put_Line ("Create step definition package : " & Step_Pkg);
      else
         Logger.Put_Line ("Update step definition package : " & Step_Pkg);
      end if;

      --  Update step

      J := First (New_Steps);
      while Has_Element (J) loop
         Tag := Element (J);
         if    Index (Tag, "@given ") = 1 then
            Unique_Procedure_Name (S.Procedures, Prc_Name, Step_Given,
                                   Slice (Tag, 8, Length (Tag)));
         elsif Index (Tag, "@when ")  = 1 then
            Unique_Procedure_Name (S.Procedures, Prc_Name, Step_When,
                                   Slice (Tag, 7, Length (Tag)));
         elsif Index (Tag, "@then ")  = 1 then
            Unique_Procedure_Name (S.Procedures, Prc_Name, Step_Then,
                                   Slice (Tag, 7, Length (Tag)));
         else
            --  GCOV_IGNORE_BEGIN
            --  Should not enter here, the tag should always start with either
            --  a @given, @when, or @then, but we never know
            Unique_Procedure_Name (S.Procedures, Prc_Name, To_String (Tag));
            --  GCOV_IGNORE_END
         end if;
         Append (Procedures, Prc_Name);
         Append (Tags,       Tag);
         Next (J);
      end loop;

      Write_Adb_File (File_Name_Adb, Step_Pkg, Procedures, Logger);
      Write_Ads_File (File_Name_Ads, Step_Pkg, Procedures, Tags, Logger);

      S.Parse (Logger);

   end Add_Steps;

   ------------
   --  Find  --
   ------------

   function  Find      (S       : in     Ada_Step_File_Type;
                        Stanza  : in     Step_Type)
                                  return Step_Match_Type
   is
      use Match_Vectors;
      Result   : Step_Match_Type;
      Step     : Step_Definition_Type;
      Matches2 : Match_Vectors.Vector;
   begin

      --  Error if not parsed
      if not S.Parsed then
         raise Unparsed_Step;
      end if;

      --  Look for the phrase
      for i in S.Steps.First_Index .. S.Steps.Last_Index loop
         Step  := S.Steps.Element (i);
         if Step.Prefix = Stanza.Kind then
            declare
               Matched  : Match_Array (0 .. Paren_Count (Step.Pattern_R.all));
            begin
--                Put_Line ("AdaSpec.Steps.Ada.Find: Match """ &
--                          To_String (Stanza.Stanza) & """ against |" &
--                          To_String (Step.Pattern_S) & "|");
               Match (Step.Pattern_R.all, Stanza.Stanza, Matched);
               if Matched (0) /= No_Match then
                  if Result.Match then
                     raise Ambiguous_Match;
                  end if;
--                   Put_Line ("Matched (0) = " &
--                             Slice (Stanza.Stanza,
--                                    Matched (0).First,
--                                    Matched (0).Last));
                  Result.Proc_Name := Step.Proc_Name;
                  Result.Position  := Step.Position;
                  Result.Match     := True;
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
                  Result.Matches := Matches2;
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
      return Result;
   end Find;


   ----------------
   --  Finalize  --
   ----------------

   overriding procedure Finalize  (S : in out Ada_Step_File_Type) is
   begin
      Finalize (S.Steps);
   end Finalize;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize (Steps : in out Step_Container.Vector) is
      use Step_Container;
      I : Step_Container.Cursor := First (Steps);
      E : Step_Definition_Type;
   begin
      while Has_Element (I) loop
         E := Element (I);
         Free (E.Pattern_R);
         Next (I);
      end loop;
      Clear (Steps);
   end Finalize;



end AdaSpec.Step_Definitions.Ada05;
