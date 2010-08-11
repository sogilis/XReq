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

with Util.Strings;
with XReq.Result_Features;
with XReq.Step_Definitions;

use Util.Strings;
use XReq.Result_Features;
use XReq.Step_Definitions;

package body XReq.Job is

   --------------------------
   --  Job_Type  --  Make  --
   --------------------------

   procedure Make (Job          : out    Job_Type;
                   Feature_File : in     String) is
   begin
      Job := (
         Feature_File => To_Unbounded_String (Feature_File),
         others   => <>);
   end Make;

   ----------------------------------
   --  Job_Type  --  Feature_File  --
   ----------------------------------

   function  Feature_File (Job : in Job_Type) return String is begin
      return To_String (Job.Feature_File);
   end Feature_File;

   -------------------------
   --  Job_Type  --  Run  --
   -------------------------

   procedure Run (Job           : in out Job_Type;
                  Env           : in out Job_Environment;
                  Logger        : in     Logger_Ptr;
                  Add_Steps_Pkg : in     String  := "";
                  Step_Matching : in     Boolean := False)
   is
      use String_Sets;
      F : constant Feature_File_Ptr := new Feature_File_Type;
      Missing_Steps : String_Set;
      S : Step_Definitions_Ptr;
   begin
      if not Env.Loaded then
         raise Invalid_Environment with "Must call Load (Env) first";
      end if;

      F.Make (Feature_File (Job));
      Job.Feature := Generic_Feature_Ptr (F);

      F.Parse (Logger);

      --  No Parse_Error
      Logger.Put_Line ("Compile: " & To_String (Job.Feature_File));
      Job.Result.Process_Feature (Job.Feature, Env.Steps, Logger,
                                  Missing_Steps, Step_Matching);

      if Add_Steps_Pkg /= "" and not Is_Empty (Missing_Steps) then
         Env.Steps (S);
         Add_Steps (S.all, Missing_Steps, Add_Steps_Pkg,
                      First_Step_Dir (Env), Env.Language, Logger);
         Clear (Missing_Steps);
         Job.Result.Set_Fail (False);
         Job.Result.Process_Feature (Job.Feature, Env.Steps, Logger,
                                     Missing_Steps, Step_Matching);
      end if;

   exception
      when Parse_Error =>
         Job.Result.Set_Fail;
   end Run;

   -----------------------------
   --  Job_Type  --  Cleanup  --
   -----------------------------

   procedure Cleanup (Job : in out Job_Type) is
   begin
      Free (Feature_Ptr (Job.Feature));
   end Cleanup;

   ------------
   --  Init  --
   ------------

   procedure Init (Env          : out    Job_Environment;
                   Job          : out    Job_Type;
                   Logger       : in     Logger_Ptr;
                   Feature_File : in     String;
                   Step_Dir     : in     String_Vector :=
                                         Empty_String_Vector;
                   Out_Dir      : in     String := "")
   is
      E : Job_Environment;
      J : Job_Type;
   begin
      Make (J, Feature_File);
      Make (E, Step_Dir, Out_Dir);
      Fill_Missing (E, Feature_File);
      Load (E, Logger);
      Env := E;
      Job := J;
   end Init;


end XReq.Job;
