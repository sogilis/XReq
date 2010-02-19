--                         Copyright (C) 2010, Sogilis                       --

with Ada.Directories;
with Util.IO;

use Ada.Directories;
use Util.IO;

package body AdaSpec.Generator.Ada is


   procedure Generate_Step     (S        : in out Ada_Generator_Type;
                                Step     : in     Result_Step_Type);
   procedure Generate_Scenario (S        : in out Ada_Generator_Type;
                                Scenario : in     Result_Scenario_Type;
                                Name     : in     Unbounded_String);
   procedure Generate_Feature  (S        : in out Ada_Generator_Type);
   procedure Generate_With     (S        : in out Ada_Generator_Type);

   ------------
   --  Make  --
   ------------

   procedure Make     (Gen : out    Ada_Generator_Type;
                       Job : in     Job_Type;
                       Env : in     Job_Environment)
   is

      Pkgname     : constant String := Base_Name (Feature_File (Job));
      Basename    : constant String := Compose (Out_Dir (Env), Pkgname);
      Generator   : Ada_Generator_Type := (
                     Feature  => Job.Result,
                     Ads_File => To_Unbounded_String (Basename & ".ads"),
                     Adb_File => To_Unbounded_String (Basename & ".adb"),
                     others   => <>);
   begin
      Get_Unique_String (
         Gen.Pool, To_Identifier (Pkgname),      Generator.Id_Pkgname);
      Get_Unique_String (
         Gen.Pool, To_Identifier ("Background"), Generator.Fn_Backgnd);
      Gen := Generator;
   end Make;

   ----------------
   --  Generate  --
   ----------------

   procedure Generate (Gen : in out Ada_Generator_Type) is
      use Util.Strings.Vectors;
   begin
      Gen.Ads_Line ("package " & Gen.Id_Pkgname & " is");
      Gen.Adb_Line ("package body " & Gen.Id_Pkgname & " is");
      Gen.Indent;
      Generate_Feature (Gen);
      Gen.Ads_Line ("procedure Run;");
      Gen.Adb_Line ("procedure Run is");
      Gen.Adb_Line ("begin");
      Gen.Indent;
      for I in 0 .. Integer (Length (Gen.Fn_Steps)) - 1 loop
         Gen.Adb_Line (Gen.Fn_Backgnd & ";");
         Gen.Adb_Line (Element (Gen.Fn_Steps, I) & ";");
      end loop;
      Gen.Unindent;
      Gen.Adb_Line ("end Run;");
      Gen.Unindent;
      Gen.Ads_Line ("end " & Gen.Id_Pkgname & ";");
      Gen.Adb_Line ("end " & Gen.Id_Pkgname & ";");
      Generate_With (Gen);

      Set_File (To_String (Gen.Ads_File), To_String (Gen.Ads_Buf));
      Set_File (To_String (Gen.Adb_File), To_String (Gen.Adb_Buf));
   end Generate;


   ----------------------
   --  Output adb/ads  --
   ----------------------

   procedure Adb_Line (State : in out Ada_Generator_Type; Line : in String)
   is begin
      Append (State.Adb_Buf, State.Ind_Adb & Line & State.CRLF);
   end Adb_Line;
   procedure Ads_Line (State : in out Ada_Generator_Type; Line : in String)
   is begin
      Append (State.Ads_Buf, State.Ind_Ads & Line & State.CRLF);
   end Ads_Line;
--    procedure Adb (State : in out Ada_Generator_Type; S : in String) is begin
--       Append (State.Ads_Buf, S);
--    end Adb;
--    procedure Ads (State : in out Ada_Generator_Type; S : in String) is begin
--       Append (State.Ads_Buf, S);
--    end Ads;

   procedure Adb_Line (State : in out Ada_Generator_Type;
                       Line  : in     Unbounded_String)
   is begin
      Append (State.Adb_Buf, State.Ind_Adb & Line & State.CRLF);
   end Adb_Line;
   procedure Ads_Line (State : in out Ada_Generator_Type;
                       Line  : in     Unbounded_String)
   is begin
      Append (State.Ads_Buf, State.Ind_Ads & Line & State.CRLF);
   end Ads_Line;
--    procedure Adb (State : in out Ada_Generator_Type;
--                   S     : in     Unbounded_String) is
--    begin
--       Append (State.Ads_Buf, S);
--    end Adb;
--    procedure Ads (State : in out Ada_Generator_Type;
--                   S     : in     Unbounded_String) is
--    begin
--       Append (State.Ads_Buf, S);
--    end Ads;

   ------------------------
   --  Indent, Unindent  --
   ------------------------

   procedure Indent_Ads (State : in out Ada_Generator_Type;
                         N     : in     Positive := 3) is
   begin
      Append (State.Ind_Ads, N * " ");
   end Indent_Ads;

   procedure Indent_Adb (State : in out Ada_Generator_Type;
                         N     : in     Positive := 3) is
   begin
      Append (State.Ind_Adb, N * " ");
   end Indent_Adb;

   procedure Unindent_Ads (State : in out Ada_Generator_Type;
                           N     : in Positive := 3) is
   begin
      Head (State.Ind_Ads, Length (State.Ind_Ads) - N);
   end Unindent_Ads;

   procedure Unindent_Adb (State : in out Ada_Generator_Type;
                           N     : in Positive := 3) is
   begin
      Head (State.Ind_Adb, Length (State.Ind_Adb) - N);
   end Unindent_Adb;

   procedure Indent (State : in out Ada_Generator_Type;
                     N     : in     Positive := 3) is
   begin
      Indent_Ads (State, N);
      Indent_Adb (State, N);
   end Indent;

   procedure Unindent (State : in out Ada_Generator_Type;
                       N     : in     Positive := 3) is
   begin
      Unindent_Ads (State, N);
      Unindent_Adb (State, N);
   end Unindent;

   ---------------------
   --  Generate_Step  --
   ---------------------

   procedure Generate_Step     (S        : in out Ada_Generator_Type;
                                Step     : in Result_Step_Type)
   is
      use String_Set;
      Procname : constant String := Procedure_Name (Step);
      Pkgname  : Unbounded_String;
      Copy     : Boolean := False;
   begin
      S.Adb_Line (Procname & ";");
      for K in reverse Procname'Range loop
         if Copy then
            Pkgname := Procname (K) & Pkgname;
         elsif Procname (K) = '.' then
            Copy := True;
         end if;
      end loop;
      if not Contains (S.With_Pkg, Pkgname) then
         Insert (S.With_Pkg, Pkgname);
      end if;
   end Generate_Step;

   -------------------------
   --  Generate_Scenario  --
   -------------------------

   procedure Generate_Scenario (S        : in out Ada_Generator_Type;
                                Scenario : in Result_Scenario_Type;
                                Name     : in Unbounded_String)
   is
      use Result_Steps;
      I : Result_Steps.Cursor := First (Scenario.Steps);
   begin
      S.Ads_Line ("procedure " & Name & ";");
      S.Adb_Line ("procedure " & Name & " is");
      S.Adb_Line ("begin");
      S.Indent_Adb;
      while Has_Element (I) loop
         Generate_Step (S, Element (I));
         Next (I);
      end loop;
      S.Unindent_Adb;
      S.Adb_Line ("end " & Name & ";");
   end Generate_Scenario;

   ------------------------
   --  Generate_Feature  --
   ------------------------

   procedure Generate_Feature  (S : in out Ada_Generator_Type)
   is
      use Util.Strings.Vectors;
      use Result_Scenarios;
      I   : Result_Scenarios.Cursor := First (S.Feature.Scenarios);
      Str : Unbounded_String;
   begin
      Generate_Scenario (S, S.Feature.Background, S.Fn_Backgnd);
      while Has_Element (I) loop
         Get_Unique_String (S.Pool,
            To_Identifier ("Scenario_" & To_String (Element (I).Name)),
            Str);
         Append (S.Fn_Steps, Str);
         Generate_Scenario (S, Element (I), Str);
         Next (I);
      end loop;
   end Generate_Feature;

   ---------------------
   --  Generate_With  --
   ---------------------

   procedure Generate_With (S : in out Ada_Generator_Type)
   is
      use String_Set;
      J   : String_Set.Cursor := First (S.With_Pkg);
      Buf : Unbounded_String;
   begin
      while Has_Element (J) loop
         Append (Buf, "with " & Element (J) & ";" & S.CRLF);
         Next (J);
      end loop;
      S.Adb_Buf := Buf & S.CRLF & S.Adb_Buf;
   end Generate_With;


end AdaSpec.Generator.Ada;
