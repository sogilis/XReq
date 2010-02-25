--                         Copyright (C) 2010, Sogilis                       --

with Ada.Directories;
with Util.IO;

use Ada.Directories;
use Util.IO;

package body AdaSpec.Generator.Ada is


   procedure Generate_Step     (S          : in out Ada_Generator_Type;
                                Step       : in     Result_Step_Type);
   procedure Generate_Scenario (S          : in out Ada_Generator_Type;
                                Scenario   : in     Result_Scenario_Type;
                                Name       : in     Unbounded_String;
                                Background : in Boolean := False);
   procedure Generate_Feature  (S          : in out Ada_Generator_Type);
   procedure Generate_With     (S          : in out Ada_Generator_Type);

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
      Gen.Adb.Put_Line ("with AdaSpecLib;");
      Gen.Adb.Put_Line ("with AdaSpecLib.Format.Text;");
      Gen.Adb.Put_Line ("use  AdaSpecLib;");
      Gen.Adb.Put_Line ("use  AdaSpecLib.Format.Text;");
      Gen.Ads.Put_Line ("package " & Gen.Id_Pkgname & " is");
      Gen.Adb.Put_Line ("package body " & Gen.Id_Pkgname & " is");
      Indent (Gen.Ads);
      Indent (Gen.Adb);
      Generate_Feature (Gen);
      Gen.Ads.Put_Line ("procedure Run;");
      Gen.Adb.Put_Line ("procedure Run is");
      Gen.Adb.Put_Line ("begin");
      Indent (Gen.Ads);
      Indent (Gen.Adb);
      Gen.Adb.Put_Line ("Put_Feature (" &
                        Ada_String (To_String (Gen.Feature.Name)) & ");");
      for I in 0 .. Integer (Length (Gen.Fn_Steps)) - 1 loop
         Gen.Adb.Put_Line (Gen.Fn_Backgnd & ";");
         Gen.Adb.Put_Line (Element (Gen.Fn_Steps, I) & ";");
      end loop;
      Gen.Ads.UnIndent;
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("end Run;");
      Gen.Ads.UnIndent;
      Gen.Adb.UnIndent;
      Gen.Ads.Put_Line ("end " & Gen.Id_Pkgname & ";");
      Gen.Adb.Put_Line ("end " & Gen.Id_Pkgname & ";");
      Generate_With (Gen);

      Set_File (To_String (Gen.Ads_File), To_String (Gen.Ads.Buffer));
      Set_File (To_String (Gen.Adb_File), To_String (Gen.Adb.Buffer));
   end Generate;

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
      --  Declare
      S.Adb.Put_Line ("declare");
      S.Adb.Indent;
      S.Adb.Put_Line ("Args : Arg_Type;");
      S.Adb.UnIndent;
      S.Adb.Put_Line ("begin");
      S.Adb.Indent;
      --  Generate arguments
      S.Adb.Put_Line ("Make (Args, " &
                      Ada_String (To_String (Step.Step.Stanza)) & ");");
      --  Generate with clause
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
      --  Call to step
      S.Adb.Put_Line (Procname & " (Args);");
      --  Print the step
      S.Adb.Put_Indent;
      S.Adb.Put ("Put_Step (");
      case Step.Step.Prefix is
         when Prefix_Given => S.Adb.Put ("Step_Given");
         when Prefix_When  => S.Adb.Put ("Step_When");
         when Prefix_Then  => S.Adb.Put ("Step_Then");
      end case;
      S.Adb.Put (", " & Ada_String (To_String (Step.Step.Stanza)) & ");");
      S.Adb.New_Line;
      --  End block
      S.Adb.UnIndent;
      S.Adb.Put_Line ("end;");
   end Generate_Step;

   -------------------------
   --  Generate_Scenario  --
   -------------------------

   procedure Generate_Scenario (S          : in out Ada_Generator_Type;
                                Scenario   : in Result_Scenario_Type;
                                Name       : in Unbounded_String;
                                Background : in Boolean := False)
   is
      use Result_Steps;
      I : Result_Steps.Cursor := First (Scenario.Steps);
   begin
      S.Ads.Put_Line ("procedure " & Name & ";");
      S.Adb.Put_Line ("procedure " & Name & " is");
      S.Adb.Put_Line ("begin");
      Indent (S.Adb);
      S.Adb.Put_Indent;
      if Background then
         S.Adb.Put ("Put_Background ");
      else
         S.Adb.Put ("Put_Scenario ");
      end if;
      S.Adb.Put ("(" & Ada_String (To_String (Scenario.Name)) & ");");
      S.Adb.New_Line;
      while Has_Element (I) loop
         Generate_Step (S, Element (I));
         Next (I);
      end loop;
      S.Adb.UnIndent;
      S.Adb.Put_Line ("end " & Name & ";");
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
      Generate_Scenario (S, S.Feature.Background, S.Fn_Backgnd, True);
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
         Append (Buf, "with " & Element (J) & ";" & S.Adb.CRLF);
         Next (J);
      end loop;
      S.Adb.Buffer := Buf & S.Adb.CRLF & S.Adb.Buffer;
   end Generate_With;

   -----------------
   --  Full_Name  --
   -----------------

   function  Full_Name (Gen : in Ada_Generator_Type) return String is
   begin
      return To_String (Gen.Id_Pkgname);
   end Full_Name;

   ----------------------
   --  Generate_Suite  --
   ----------------------

   procedure Generate_Suite (Gens : in Generator_Vectors.Vector;
                             Name : in String;
                             Env  : in Job_Environment)
   is
      use Generator_Vectors;
      Filename : constant String := Out_Dir (Env) & "/" & Name & ".adb";
      With_B   : Buffer_Type;
      Body_B   : Buffer_Type;
      I        : Generator_Vectors.Cursor := First (Gens);
      E        : Ada_Generator_Ptr;
      Prc_Name : constant String := To_Identifier (Name);
   begin
      With_B.Put_Line ("--  File: " & Filename);
      Body_B.Put_Line ("procedure " & Prc_Name & " is");
      Body_B.Put_Line ("begin");
      Body_B.Indent;
      while Has_Element (I) loop
         E := Ada_Generator_Ptr (Element (I));
         With_B.Put_Line ("with " & E.Full_Name & ";");
         Body_B.Put_Line (E.Full_Name & ".Run;");
         Next (I);
      end loop;
      Body_B.UnIndent;
      Body_B.Put_Line ("end " & Prc_Name & ";");

      Set_File    (Filename, To_String (With_B.Buffer));
      Append_File (Filename, To_String (Body_B.Buffer));
   end Generate_Suite;

end AdaSpec.Generator.Ada;
