-------------------------------------------------------------------------------
--  XReq  --  Behaviour Driven Developpement tool for compiled languages     --
--  Copyright (c) 2010, SOGILIS <http://sogilis.com>                         --
--                                                                           --
--  This program is free software: you can redistribute it and/or modify     --
--  it under the terms of the GNU Affero General Public License as           --
--  published by the Free Software Foundation, either version 3 of the       --
--  License, or (at your option) any later version.                          --
--                                                                           --
--  This program is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            --
--  GNU Affero General Public License for more details.                      --
--                                                                           --
--  You should have received a copy of the GNU Affero General Public License --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.    --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Command_Line;
with XReqLib.Format.Text;
with XReqLib.ANSI;
with XReqLib.Error_Handling;

use Ada.Exceptions;
use Ada.Strings.Unbounded;
use Ada.Text_IO;
use GNAT.OS_Lib;
use GNAT.Command_Line;
use XReqLib.Format.Text;

package body XReqLib.CLI is

   procedure Help (Name : in String := Command_Name) is
   begin

      pragma Style_Checks (Off);

      --         0         10        20        30        40        50        60        70
      --         ---------------------------------------------------------------------------XXX

      Put_Line (Name & " - Run features");
      Put_Line ("");
      Put_Line ("SYNOPSIS");
      Put_Line ("");
      Put_Line ("    " & Name & " [OPTIONS] [FEATURE:NUM ...]");
      Put_Line ("");
      Put_Line ("OPTIONS");
      Put_Line ("");
      Put_Line ("    -h, -help, --help");
      Put_Line ("        Help message");
      Put_Line ("");
      Put_Line ("    -f, --format FORMAT");
      Put_Line ("        Choose the reporting format");
      Put_Line ("        Available formats: Text, HTML");
      Put_Line ("");
      Put_Line ("    -o, --output OUTPUT");
      Put_Line ("        File or directory used for the output. Specify - for the standard");
      Put_Line ("        output.");
      Put_Line ("");
      Put_Line ("    -t, --tags TAGS");
      Put_Line ("        Conditional execution using TAGS. Format is:");
      Put_Line ("            @tag:        execute tag @tag");
      Put_Line ("            ~@tag:       execute all but tag @tag");
      Put_Line ("            @tag1+@tag2: execute scenarios with both @tag1 and @tag2");
      Put_Line ("            @tag1,@tag2: execute scenarios with either @tag1 or @tag2");
      Put_Line ("");
      Put_Line ("    --list");
      Put_Line ("        Show all the features and scenarios in this test suite");
      Put_Line ("");
      Put_Line ("    -d, --debug");
      Put_Line ("        Produce reports with extra information to ease debugging");
      Put_Line ("");
      Put_Line ("    --no-color");
      Put_Line ("        Don't show colors on terminal output");
      Put_Line ("");
      Put_Line ("    --no-stacktrace");
      Put_Line ("        Don't show stack traces on terminal output");
      Put_Line ("");

      --         ---------------------------------------------------------------------------XXX
      --         0         10        20        30        40        50        60        70

      pragma Style_Checks (On);

   end Help;

   procedure Get_Arguments   (Args : out Argument_List_Access) is
      Count : constant Natural := Argument_Count;
   begin
      Args := new String_List (1 .. Count);
      for I in 1 .. Count loop
         Args (I) := new String'(Argument (I));
      end loop;
   end Get_Arguments;

   procedure Parse_Arguments (Args       : in out Argument_List_Access;
                              Format     : out    Format_Ptr;
                              Continue   : out    Boolean;
                              Cond       : out    Conditional_Type;
                              List_Mode  : out    Boolean;
                              Success    : out    Boolean;
                              Name       : in     String := Command_Name)
   is
      use String_Vectors;
      Parser     : Opt_Parser;
      Options    : constant String := "help h -help f: -format= o: -output= "
        & "-debug d -tags= t: -list -no-color -no-stacktrace";
      Output     : Unbounded_String := Null_Unbounded_String;
      Arg        : Unbounded_String;
      Debug_Mode : Boolean := False;
   begin
      Check_Elaboration;

      Success    := True;
      List_Mode  := False;
      Cond       := Null_Condition;
      Initialize_Option_Scan (Parser, Args);
      Args       := null;  --  Parser takes ownership
      Format     := null;
      Continue   := True;

      Getopt_Loop :
      while Getopt (Options, True, Parser) /= ASCII.NUL loop

         if Full_Switch (Parser) = "h" or
            Full_Switch (Parser) = "help" or
            Full_Switch (Parser) = "-help"
         then
            Help (Name);
            Continue := False;
            exit Getopt_Loop;

         elsif Full_Switch (Parser) = "d" or
               Full_Switch (Parser) = "-debug"
         then
            Debug_Mode := True;

         elsif Full_Switch (Parser) = "-list" then
            List_Mode := True;

         elsif Full_Switch (Parser) = "t" or
               Full_Switch (Parser) = "-tags"
         then
            Cond := Create (Parameter (Parser));

         elsif Full_Switch (Parser) = "f" or
               Full_Switch (Parser) = "-format"
         then
            Free (Format);
            Format := Get_Formatter (Parameter (Parser));
            if Format = null then
               Put_Line (Current_Error,
                         "Invalid format: " & Parameter (Parser));
            end if;

         elsif Full_Switch (Parser) = "o" or
               Full_Switch (Parser) = "-output"
         then
            Output := To_Unbounded_String (Parameter (Parser));

         elsif Full_Switch (Parser) = "-no-color" then
            XReqLib.ANSI.Use_ANSI_Sequences := False;

         elsif Full_Switch (Parser) = "-no-stacktrace" then
            XReqLib.Error_Handling.Show_Traces := False;

         else  --  Never happen unless a bug in Getopt     --  GCOV_IGNORE
            raise Invalid_Switch;                          --  GCOV_IGNORE

         end if;

      end loop Getopt_Loop;

      Arg := To_Unbounded_String (Get_Argument (False, Parser));
      while Length (Arg) > 0 loop
         Append (Cond.Scenarios, Arg);
         Arg := To_Unbounded_String (Get_Argument (False, Parser));
      end loop;

      Free (Parser);


      if Continue then
         if Format = null then
            Format := Format_Ptr (New_Text_Format);
         end if;
         if Output /= Null_Unbounded_String then
            Format.Set_Output (To_String (Output));
         end if;
         Format.Set_Debug (Debug_Mode);
      else
         Free (Format);
      end if;
   exception
      when Invalid_Switch =>
         Put_Line (Current_Error,
                  "Invalid switch: -" & Full_Switch (Parser) & " " &
                  Parameter (Parser));
         Free (Parser);
         Free (Format);
         Help (Name);
         Success    := False;
         Continue   := False;
         List_Mode  := False;
         Cond       := Null_Condition;
      when Invalid_Parameter =>
         Put_Line (Current_Error, "Missing parameter for switch -" &
                  Full_Switch (Parser));
         Free (Parser);
         Free (Format);
         Help (Name);
         Success    := False;
         Continue   := False;
         List_Mode  := False;
         Cond       := Null_Condition;
      --  GCOV_IGNORE_BEGIN
      when E : others =>
         Success    := False;
         Format     := null;
         Continue   := False;
         List_Mode  := False;
         Cond       := Null_Condition;
         Free (Parser);
         Free (Format);
         Reraise_Occurrence (E);
      --  GCOV_IGNORE_END
   end Parse_Arguments;

   procedure Parse_Arguments (Format     : out    Format_Ptr;
                              Continue   : out    Boolean;
                              Cond       : out    Conditional_Type;
                              List_Mode  : out    Boolean;
                              Name       : in     String := Command_Name)
   is
      Args : Argument_List_Access;
      Succ : Boolean;
   begin
      Get_Arguments   (Args);
      Parse_Arguments (Args, Format, Continue, Cond, List_Mode, Succ, Name);
      if not Succ then
         Set_Exit_Status (Failure);
      end if;
   end Parse_Arguments;

end XReqLib.CLI;
