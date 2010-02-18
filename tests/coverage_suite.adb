--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Util.IO;
with AUnit.Assertions;

use Ada.Text_IO;
use Ada.Directories;
use Ada.Strings.Unbounded;
use AUnit.Assertions;

package body Coverage_Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite is

      Ret : constant AUnit.Test_Suites.Access_Test_Suite :=
            new AUnit.Test_Suites.Test_Suite;
      Search       : Search_Type;
      Item         : Directory_Entry_Type;
      Current_Test : access Test;

   begin

      Put_Line ("The coverage test expect the .gcov files to be in the");
      Put_Line ("subdirectory `reports' of the current directory.");

      Start_Search (Search,
                    Directory => Compose (Current_Directory, "reports"),
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

   end Suite;



   function PkgName (T : in Test) return String is
      File : constant String := To_String (T.File);
      Name : Unbounded_String;
   begin
      if File (File'Length - 4 .. File'Length) = ".gcov" then
         Name := To_Unbounded_String (File (File'First .. File'Length - 5));
      else
         Name := To_Unbounded_String (File);
      end if;
      declare
         Name2 : String := To_String (Name);
      begin
         for i in Name2'Range loop
            if Name2 (i) = '-' then
               Name2 (i) := '.';
            end if;
         end loop;
         return Name2;
      end;
   end PkgName;



   function Name (T : in Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("Coverage " & PkgName (T));
   end Name;



   procedure Run_Test (T : in out Test) is

      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Util.IO;

      type Percent is delta 0.01 range 0.00 .. 100.00;

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

      Assert (Count /= 0 or Ignored /= 0,
              "File: reports/" & To_String (T.File) & " " &
              "error, non executable file" &
              CRLF & Read_Whole_File (File_Path));

      if Count = 0 then
         Ratio   := 100.00;
      else
         Ratio_F := 100.0 * Float (Covered) / Float (Count);
         Ratio   := Percent (Ratio_F);
      end if;

      Assert (Error <= 0,
              "File: reports/" & To_String (T.File) & " error line" &
              Integer'Image (Error) &
              CRLF & Read_Whole_File (File_Path));

      Assert (Covered = Count,
              "File: reports/" & To_String (T.File) & Percent'Image (Ratio) &
              "% covered (" & Trim (Natural'Image (Covered), Left) & "/" &
              Trim (Natural'Image (Count), Left) & ")" &
              CRLF & Read_Whole_File (File_Path));

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

         if Index (Line, "BEGIN_GCOV_IGNORE") in Line'Range then
            Ignore := True;
         end if;
         if Index (Line, "END_GCOV_IGNORE") in Line'Range then
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
