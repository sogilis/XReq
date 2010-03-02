--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Ada.Containers.Hashed_Maps;
with Util.IO;

use Ada.Text_IO;
use Ada.Directories;
use Ada.Strings.Unbounded;
use Ada.Exceptions;
use Ada.Containers;


package body Coverage_Suite is

   function  Pkg_Name (File : in String) return String;
   procedure Add_LCov_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite);
   function  Different (Left, Right : in LCov_Matches_Maps.Map) return Boolean;

   type Percent is delta 0.01 range 0.00 .. 100.00;

   package LCov_Maps is new Hashed_Maps (
      Unbounded_String, LCov_Matches_Maps.Map, Hash, "=", Different);

   ----------------------------------------------------------------------------

   function Different (Left, Right : in LCov_Matches_Maps.Map) return Boolean
   is
      pragma Unreferenced (Left, Right);
   begin
      return False;
   end Different;


   function Pkg_Name (File : in String) return String is
      Name : String := File;
   begin
      for i in Name'Range loop
         if Name (i) = '-' then
            Name (i) := '.';
         end if;
      end loop;
      return Name;
   end Pkg_Name;

   procedure Add_LCov_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use LCov_Maps;
      use LCov_Matches_Maps;

      LCov_File     : File_Type;
      LCov_Map      : LCov_Maps.Map;
      File_Name     : Unbounded_String := Null_Unbounded_String;
      Lines         : LCov_Matches_Maps.Map;
      Comma         : Natural;
      DA_Line       : Natural;
      DA_Count      : Natural;
      I             : LCov_Maps.Cursor;
      Current_Test  : LCov_Test_Ptr := null;

      procedure Put_Lines;
      procedure Put_Lines is
      begin
         if File_Name /= Null_Unbounded_String then
            if Contains (LCov_Map, File_Name) then
               Replace (LCov_Map, File_Name, Lines);
            else
               Insert  (LCov_Map, File_Name, Lines);
            end if;
         end if;
      end Put_Lines;

   begin
      Open (LCov_File, In_File, "coverage/lcov.info");
      while not End_Of_File (LCov_File) loop
         declare
            Line : constant String := Get_Line (LCov_File);
         begin
            if Line (1 .. 3) = "SF:" then
               Put_Lines;
               File_Name := To_Unbounded_String (Line (4 .. Line'Last));
               if Contains (LCov_Map, File_Name) then
                  Lines := Element (LCov_Map, File_Name);
               else
                  Lines := LCov_Matches_Maps.Empty_Map;
               end if;
            elsif Line (1 .. 3) = "DA:" then
               Comma    := Index (Line, ",");
               DA_Line  := Natural'Value (Line (4 .. Comma - 1));
               DA_Count := Natural'Value (Line (Comma + 1 .. Line'Last));
               if Contains (Lines, DA_Line) then
                  if not Element (Lines, DA_Line) then
                     Replace (Lines, DA_Line, DA_Count > 0);
                  end if;
               else
                  Insert  (Lines, DA_Line, DA_Count > 0);
               end if;
            end if;
         end;
      end loop;
      Close (LCov_File);
      Put_Lines;

      I := First (LCov_Map);
      while Has_Element (I) loop
         Current_Test := new LCov_Test;
         Current_Test.all.File_Name :=
            To_Unbounded_String (Simple_Name (To_String (Key (I))));
         Current_Test.all.Lines     := Element (I);
         Ret.Add_Test (Current_Test);
         Next (I);
      end loop;

   exception
      when Error : Ada.IO_Exceptions.Name_Error =>
         Put_Line ("Could not locate LCov info file");
         Put_Line (Exception_Message (Error));
         null;
   end Add_LCov_Tests;

   ----------------------------------------------------------------------------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite :=
            new AUnit.Test_Suites.Test_Suite;
      Search        : Search_Type;
      Item          : Directory_Entry_Type;
      Current_Test  : access Test;

   begin

      New_Line;
      Put_Line ("-----------------------------------------" &
                "-----------------------------------------");
      Put_Line ("The coverage test suite expect a file in " &
                "`coverage/lcov.info' and can additionally");
      Put_Line ("read .gcov files in the coverage directory.");
      Put_Line ("-----------------------------------------" &
                "-----------------------------------------");
      New_Line;

      --  LCov  -------------------------------------------

      Add_LCov_Tests (Ret);

      --  GCov  -------------------------------------------

      Start_Search (Search,
                    Directory => Compose (Current_Directory, "coverage"),
                    Pattern   => "*.gcov",
                    Filter    => (Ordinary_File => True, others => False));

      while More_Entries (Search) loop

         Get_Next_Entry (Search, Item);

         Current_Test := new Test'(AUnit.Simple_Test_Cases.Test_Case with
            File => To_Unbounded_String (Simple_Name (Item)),
            Path => To_Unbounded_String (Full_Name   (Item)));

--          Put_Line ("1 " & Simple_Name (Item));
--          Put_Line ("2 " & To_String (Current_Test.File));
--          Put_Line ("3 " & Name (Current_Test.all).all);

         --  TODO: For some reason, I must assign again File and Path, else it
         --  doesn't work.

         Current_Test.all.File := To_Unbounded_String (Simple_Name (Item));
         Current_Test.all.Path := To_Unbounded_String (Full_Name   (Item));

--          Put_Line ("4 " & To_String (Current_Test.File));
--          Put_Line ("5 " & Name (Current_Test.all).all);

         Ret.Add_Test (Current_Test);

      end loop;

      End_Search (Search);

      return Ret;

   exception
      when Error : others =>
         Put_Line ("Error in procedure Suite");
         Put_Line (Exception_Information (Error));
         Reraise_Occurrence (Error);
   end Suite;

   --  LCov_Test  -------------------------------------------------------------

   function Name (T : in LCov_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("LCoverage " &
                           Pkg_Name (To_String (T.File_Name)));
   end Name;

   procedure Run_Test (T : in out LCov_Test) is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use LCov_Matches_Maps;
      Covered : Natural := 0;
      Total   : Natural := 0;
      I       : LCov_Matches_Maps.Cursor := First (T.Lines);
      Ratio   : Percent;
   begin
   --   Put_Line ("File " & To_String (T.File_Name));
      while Has_Element (I) loop
         Total := Total + 1;
         if Element (I) then
            Covered := Covered + 1;
   --         Put_Line ("Line" & Key (I)'Img & " covered");
   --      else
   --         Put_Line ("Line" & Key (I)'Img & " not covered");
         end if;
         Next (I);
      end loop;
      Ratio := Percent (100 * Covered / Total);
      T.Assert (Covered = Total,
                "File: " & To_String (T.File_Name) & " " &
                Trim (Percent'Image (Ratio), Left) & "% covered (" &
                Trim (Natural'Image (Covered), Left) & "/" &
                Trim (Natural'Image (Total), Left) & ")");
   end Run_Test;

   --  GCov_Test  -------------------------------------------------------------

   function PkgName (T : in Test) return String is
      File : constant String := To_String (T.File);
   begin
      if File (File'Length - 4 .. File'Length) = ".gcov" then
         return Pkg_Name (File (File'First .. File'Length - 5));
      else
         return Pkg_Name (File);
      end if;
   end PkgName;



   function Name (T : in Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("GCoverage " & PkgName (T));
   end Name;



   procedure Run_Test (T : in out Test) is

      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Util.IO;

      CRLF      : constant String := ASCII.CR & ASCII.LF;
      File_Path : constant String := To_String (T.Path);
      Count     : Natural;
      Covered   : Natural;
      Ignored   : Natural;
      Error     : Integer;
      Ratio_F   : Float;
      Ratio     : Percent;
   begin

      Read_Gcov (File_Path, Count, Covered, Ignored, Error);

      T.Assert (Count /= 0 or Ignored /= 0,
              "File: coverage/" & To_String (T.File) & " " &
              "error, non executable file" &
              CRLF & Read_Whole_File (File_Path));

      if Count = 0 then
         Ratio   := 100.00;
      else
         Ratio_F := 100.0 * Float (Covered) / Float (Count);
         Ratio   := Percent (Ratio_F);
      end if;

      T.Assert (Error <= 0,
              "File: coverage/" & To_String (T.File) & " error line" &
              Integer'Image (Error)
              --  & CRLF & Read_Whole_File (File_Path));
              );

      T.Assert (Covered = Count,
              "File: coverage/" & To_String (T.File) & Percent'Image (Ratio) &
              "% covered (" & Trim (Natural'Image (Covered), Left) & "/" &
              Trim (Natural'Image (Count), Left) & ")" &
              --  CRLF & Read_Whole_File (File_Path));
              --  The file content can contain invalid XML
              "");

   end Run_Test;

   procedure Read_Gcov_Line (File   : in out File_Type;
                             Status : out    Gcov_Line_Type;
                             Ignore : in out Boolean)
   is
      use Ada.Strings.Fixed;
      use Util.IO;
   begin

      declare
         Line  : constant String  := Get_Whole_Line (File);
         Found : Boolean := False;
      begin

         if Index (Line, "GCOV_IGNORE_BEGIN") in Line'Range then
            Ignore := True;
         end if;
         if Index (Line, "GCOV_IGNORE_END") in Line'Range then
            Ignore := False;
         end if;

         Loop_Characters :
         for i in Line'Range loop
            case Line (i) is
               when ' ' =>
                  null;
               when '#' =>
                  if Ignore or Index (Line, "GCOV_IGNORE") in Line'Range then
                     Put_Line ("Ignore line: " & Name (File) & ": " & Line);
                     Status := Gcov_Line_Ignored;
                  else
                     Status := Gcov_Line_Dead;
                  end if;
                  Found := True;
                  exit Loop_Characters;
               when '-' =>
                  Status := Gcov_Line_Blank;
                  Found := True;
                  exit Loop_Characters;
               when '0' .. '9' =>
                  Status := Gcov_Line_Alive;
                  Found := True;
                  exit Loop_Characters;
               when others =>
                  if Index (Line, "GCOV_IGNORE") in Line'Range then
                     Put_Line ("Ignore line: " & Name (File) & ": " & Line);
                     Status := Gcov_Line_Ignored;
                  else
                     Status := Gcov_Line_Error;
                  end if;
                  Found := True;
                  exit Loop_Characters;
            end case;
         end loop Loop_Characters;

         if not Found then
            Status := Gcov_Line_Error;
         end if;
      end;
   exception
      when Ada.IO_Exceptions.End_Error =>
         Status := Gcov_End_Of_File;
   end Read_Gcov_Line;

   procedure Read_Gcov (Filename         : in  String;
                        Out_Line_Count   : out Natural;
                        Out_Line_Covered : out Natural;
                        Out_Line_Ignored : out Natural;
                        Out_Error        : out Integer) is
      File   : File_Type;
      Status : Gcov_Line_Type := Gcov_Line_Error;
      Line_Count   : Natural :=  0;
      Line_Covered : Natural :=  0;
      Line_Ignored : Natural :=  0;
      Error        : Integer := -1;
      Line_Number  : Integer :=  1;
      Ignore       : Boolean := False;
   begin
--       Put_Line ("File: " & Filename);
      Open (File, In_File, Filename);

      while Error = -1 and Status /= Gcov_End_Of_File loop
         Read_Gcov_Line (File, Status, Ignore);
--          Put_Line ("   " & Status'Img);
         case Status is
            when Gcov_Line_Error =>
               Error := Line_Number;
            when Gcov_Line_Alive =>
               Line_Count   := Line_Count + 1;
               Line_Covered := Line_Covered + 1;
            when Gcov_Line_Dead =>
               Line_Count   := Line_Count + 1;
            when Gcov_Line_Ignored =>
               Line_Ignored := Line_Ignored + 1;
            when others =>
               null;
         end case;
         Line_Number := Line_Number + 1;
      end loop;

      Close (File);
      Out_Line_Count   := Line_Count;
      Out_Line_Covered := Line_Covered;
      Out_Line_Ignored := Line_Ignored;
      Out_Error        := Error;
   end Read_Gcov;

end Coverage_Suite;
