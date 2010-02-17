--                         Copyright (C) 2010, Sogilis                       --

with Ada.Directories;
with Util.Strings;
with Util.IO;

use Ada.Directories;
use Util.Strings;
use Util.IO;

package body AdaSpec.Generator.Ada is

   ----------------
   --  Generate  --
   ----------------

   procedure Generate (Job : in Job_Type;
                       Env : in Job_Environment)
   is

      CRLF        : constant String := "" & ASCII.LF;
      Pkgname     : constant String := Base_Name (Feature_File (Job));
      Basename    : constant String := Compose (Out_Dir (Env), Pkgname);
      Ads_File    : constant String := Basename & ".ads";
      Adb_File    : constant String := Basename & ".adb";
      Ads_Buf     : Unbounded_String;
      Adb_Buf     : Unbounded_String;
      Pool        : String_Pool;

      Idf_Pkgname : Unbounded_String;

   begin
      Get_Unique_String (Pool, To_Identifier (Pkgname), Idf_Pkgname);

      Generate_With    (Adb_Buf, Job.Result);
      Append (Ads_Buf, "package " & Idf_Pkgname & " is" & CRLF);
      Append (Adb_Buf, "package body " & Idf_Pkgname & " is" & CRLF);
      Generate_Feature (Adb_Buf, Ads_Buf, Pool, Job.Result, "   ", CRLF);
      Append (Ads_Buf, "end " & Idf_Pkgname & ";" & CRLF);
      Append (Adb_Buf, "end " & Idf_Pkgname & ";" & CRLF);

      Set_File (Ads_File, To_String (Ads_Buf));
      Set_File (Adb_File, To_String (Adb_Buf));
   end Generate;

   ---------------------
   --  Generate_Step  --
   ---------------------

   procedure Generate_Step     (Buffer   : in out Unbounded_String;
                                Pool     : in out String_Pool;
                                Step     : in Result_Step_Type;
                                Indent   : in String := "";
                                CRLF     : in String := ASCII.CR & ASCII.LF)
   is
      pragma Unreferenced (Pool);
   begin
      Append (Buffer, Indent & Procedure_Name (Step) & ";" & CRLF);
   end Generate_Step;

   -------------------------
   --  Generate_Scenario  --
   -------------------------

   procedure Generate_Scenario (Adb_Buf  : in out Unbounded_String;
                                Ads_Buf  : in out Unbounded_String;
                                Pool     : in out String_Pool;
                                Scenario : in Result_Scenario_Type;
                                Indent   : in String := "";
                                CRLF     : in String := ASCII.CR & ASCII.LF)
   is
      use Result_Steps;
      I         : Result_Steps.Cursor := First (Scenario.Steps);
      S_Name    : constant String := To_String (Scenario.Name);
      Idf_SName : Unbounded_String;
   begin
      Get_Unique_String (Pool, To_Identifier (S_Name), Idf_SName);

      Append (Ads_Buf, Indent & "procedure " & Idf_SName & ";" & CRLF);
      Append (Adb_Buf, Indent & "procedure " & Idf_SName & " is" & CRLF);
      Append (Adb_Buf, Indent & "begin" & CRLF);
      while Has_Element (I) loop
         Generate_Step (Adb_Buf, Pool, Element (I), Indent & "   ", CRLF);
         Next (I);
      end loop;
      Append (Adb_Buf, Indent & "end " & Idf_SName & ";" & CRLF);
   end Generate_Scenario;

   ------------------------
   --  Generate_Feature  --
   ------------------------

   procedure Generate_Feature  (Adb_Buf  : in out Unbounded_String;
                                Ads_Buf  : in out Unbounded_String;
                                Pool     : in out String_Pool;
                                Feature  : in Result_Feature_Type;
                                Indent   : in String := "";
                                CRLF     : in String := ASCII.CR & ASCII.LF)
   is
      use Result_Scenarios;
      I      : Result_Scenarios.Cursor := First (Feature.Scenarios);
   begin
      --  TODO: use strings pool
      while Has_Element (I) loop
         Generate_Scenario (Adb_Buf, Ads_Buf, Pool, Element (I),
                            Indent, CRLF);
         Next (I);
      end loop;
   end Generate_Feature;

   ---------------------
   --  Generate_With  --
   ---------------------

   procedure Generate_With     (Adb_Buf  : in out Unbounded_String;
                                Feature  : in Result_Feature_Type;
                                Indent   : in String := "";
                                CRLF     : in String := ASCII.CR & ASCII.LF)
   is
      use Result_Scenarios;
      use Result_Steps;
      I : Result_Scenarios.Cursor := First (Feature.Scenarios);
      V : Result_Steps.Vector;
   begin
      while Has_Element (I) loop
         V := Element (I).Steps;
         for J in First_Index (V) .. Last_Index (V) loop
            declare
               PN  : constant String := Procedure_Name (Element (V, J));
               PKG : Unbounded_String;
               Cpy : Boolean := False;
            begin
               for K in reverse PN'Range loop
                  if Cpy then
                     PKG := PN (K) & PKG;
                  elsif PN (K) = '.' then
                     Cpy := True;
                  end if;
               end loop;
               Append (Adb_Buf, Indent & "with " & PKG & ";" & CRLF);
            end;
         end loop;  --  GCOV_IGNORE: ???
         Next (I);
      end loop;
   end Generate_With;

end AdaSpec.Generator.Ada;
