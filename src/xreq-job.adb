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
with XReq.Features;
with XReq.Features.Files;

use Util.Strings;

package body XReq.Job is

   --------------------------
   --  Job_Type  --  Make  --
   --------------------------

   procedure Make (Job          : out    Job_Type;
                   Feature_File : in     String) is
   begin
      Job := (
         Feature_File => To_Unbounded_String (Feature_File),
         Result       => Create,
         others       => <>);
   end Make;

   ----------------------------------
   --  Job_Type  --  Feature_File  --
   ----------------------------------

   function  Feature_File (Job : in Job_Type) return String is
   begin
      return To_String (Job.Feature_File);
   end Feature_File;

   ----------------------------
   --  Job_Type  --  Result  --
   ----------------------------

   function  Result       (Job : in Job_Type) return Result_Feature_Handle is
   begin
      return Job.Result;
   end Result;

   -------------------------
   --  Job_Type  --  Run  --
   -------------------------

   procedure Run (Job           : in out Job_Type;
                  Env           : in out Environment_Handle;
                  Logger        : in     Logger_Ptr;
                  Add_Steps_Pkg : in     String  := "";
                  Step_Matching : in     Boolean := False)
   is
      use String_Sets;
      F : XReq.Features.Files.Feature_File_Ptr;
      Missing_Steps : String_Set;
   begin
      if not Env.Ref.Loaded then
         raise Environment.Invalid_Environment with "Must call Env.Load first";
      end if;

      F := new XReq.Features.Files.Feature_File_Type;
      F.Make (To_String (Job.Feature_File));
      Job.Feature.Set (XReq.Features.Feature_Ptr (F));

      F.Parse (Logger);

      --  No Parse_Error
      Logger.Put_Line ("Compile: " & To_String (Job.Feature_File));
      Job.Result.R.Process_Feature
        (Job.Feature, Env.Ref.Steps, Logger, Missing_Steps, Step_Matching);

      --  Create missing step definitions
      if Add_Steps_Pkg /= "" and not Is_Empty (Missing_Steps) then
         Env.Ref.Steps.Ref.Add_Steps (Missing_Steps, Add_Steps_Pkg,
                      Env.Ref.First_Step_Dir, Env.Ref.Language, Logger);
         Clear (Missing_Steps);
         Job.Result.R.Set_Fail (False);
         Job.Result.R.Process_Feature
           (Job.Feature, Env.Ref.Steps, Logger, Missing_Steps, Step_Matching);
      end if;

   exception
      when XReq.Features.Parse_Error =>
         Job.Result.R.Set_Fail;
   end Run;

   ------------
   --  Init  --
   ------------

   procedure Init (Env          : in out Environment_Handle;
                   Job          : in out Job_Type;
                   Logger       : in     Logger_Ptr;
                   Feature_File : in     String;
                   Step_Dir     : in     String_Vector :=
                                         Empty_String_Vector;
                   Out_Dir      : in     String := "")
   is
      E : constant Environment_Handle := Create;
      J : Job_Type;
   begin
      J.Make (Feature_File);
      E.Ref.Make (Step_Dir, Out_Dir);
      E.Ref.Fill_Missing (Feature_File);
      E.Ref.Load (Logger);
      Env := E;
      Job := J;
   end Init;


end XReq.Job;
