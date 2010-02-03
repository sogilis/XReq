--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded;
with AUnit.Assertions;

use Ada.Text_IO;
use Ada.Directories;
use Ada.Strings.Unbounded;
use AUnit.Assertions;

package body Coverage_Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite :=
            new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test (new Test);
      return Ret;
   end Suite;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Full Coverage");
   end Name;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      Search : Search_Type;
      Item   : Directory_Entry_Type;
      Count, Covered, Error : Integer;
      Ok : Boolean := True;
      Message : Unbounded_String;
   begin
      Message := To_Unbounded_String ("");

      Put_Line ("The coverage test expect the .gcov files to be in the");
      Put_Line ("subdirectory `reports' of the current directory.");

      Start_Search (Search,
                    Directory => Compose (Current_Directory, "reports"),
                    Pattern   => "*.gcov",
                    Filter    => (Ordinary_File => True, others => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Item);
         Read_Gcov (Full_Name (Item), Count, Covered, Error);
         if Error > 0 then
            declare
               s : constant String :=
                  "File: " & Simple_Name (Item) & " error line " &
                  Integer'Image (Error);
            begin
               Append (Message, s & ASCII.CR & ASCII.LF);
               Put_Line (s);
            end;
            Ok := False;
         else
            declare
               Percent : constant Float := 100.0 *
                  Float (Covered) / Float (Count);
               s       : constant String :=
                  "File: " & Simple_Name (Item) & " covered " &
                  Float'Image (Percent) & "% (" &
                  Integer'Image (Covered) & " /" &
                  Integer'Image (Count) & " )";
            begin
               Append (Message, s & ASCII.CR & ASCII.LF);
               Put_Line (s);
               Ok := False;
            end;
            Ok := Ok and Covered = Count;
         end if;
      end loop;
      End_Search (Search);

      Assert (Ok, To_String (Message));
   end Run_Test;

   procedure Read_Gcov_Line (File   : in out File_Type;
                             Status : out    Gcov_Line_Type)
   is
      Line      : String (1 .. 15);
      Last      : Natural := Line'Last;
      Found     : Boolean := False;
      First_Run : Boolean := True;
   begin
      Get_Line (File, Line, Last);
      if Last = 0 then
         Status := Gcov_End_Of_File;
      else
         while First_Run or Last = Line'Last loop
            if not First_Run then
               Get_Line (File, Line, Last);
            else
               First_Run := False;
            end if;
            --  If we already found the information we needed, don't parse the
            --  line. Else look for the first non space character to know if
            --  the line has been executed (digit), or has not ('#'). The
            --  character '-' means that the line do not contain instructions.
            if not Found then
               Loop_Characters :
               for i in Line'First .. Last loop
                  case Line (i) is
                     when ' ' =>
                        null;
                     when '#' =>
                        Status := Gcov_Line_Dead;
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
                        Status := Gcov_Line_Error;
                        Found := True;
                        exit Loop_Characters;
                  end case;
               end loop Loop_Characters;
            end if;
         end loop;
         if not Found then
            Status := Gcov_Line_Error;
         end if;
      end if;
   exception
      when End_Error =>
         Status := Gcov_End_Of_File;
   end Read_Gcov_Line;

   procedure Read_Gcov (Filename         : in  String;
                        Out_Line_Count   : out Integer;
                        Out_Line_Covered : out Integer;
                        Out_Error        : out Integer) is
      File   : File_Type;
      Status : Gcov_Line_Type := Gcov_Line_Error;
      Line_Count   : Integer :=  0;
      Line_Covered : Integer :=  0;
      Error        : Integer := -1;
      Line_Number  : Integer :=  1;
   begin
      Open (File, In_File, Filename);

      while Error = -1 and Status /= Gcov_End_Of_File loop
         Read_Gcov_Line (File, Status);
         case Status is
            when Gcov_Line_Error =>
               Error := Line_Number;
            when Gcov_Line_Alive =>
               Line_Count   := Line_Count + 1;
               Line_Covered := Line_Covered + 1;
            when Gcov_Line_Dead =>
               Line_Count   := Line_Count + 1;
            when others =>
               null;
         end case;
         Line_Number := Line_Number + 1;
      end loop;

      Close (File);
      Out_Line_Count   := Line_Count;
      Out_Line_Covered := Line_Covered;
      Out_Error        := Error;
   end Read_Gcov;

end Coverage_Suite;
