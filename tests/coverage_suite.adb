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



   function Name (T : in Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("Coverage for " & To_String (T.File));
   end Name;



   procedure Run_Test (T : in out Test) is
      type Percent is delta 0.01 range 0.00 .. 100.00;
      Count   : Natural;
      Covered : Natural;
      Error   : Integer;
      Ratio   : Percent;
   begin

      Read_Gcov (To_String (T.Path), Count, Covered, Error);
      Ratio := Percent (100.0 * Float (Covered) / Float (Count));

      Assert (Error <= 0,
              "File: " & To_String (T.File) & " error line" &
              Integer'Image (Error));

      Assert (Covered = Count,
              "File: " & To_String (T.File) & Percent'Image (Ratio) &
              "% covered (" & Natural'Image (Covered) & "/" &
              Natural'Image (Count) & ")");

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
                        Out_Line_Count   : out Natural;
                        Out_Line_Covered : out Natural;
                        Out_Error        : out Integer) is
      File   : File_Type;
      Status : Gcov_Line_Type := Gcov_Line_Error;
      Line_Count   : Natural :=  0;
      Line_Covered : Natural :=  0;
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
