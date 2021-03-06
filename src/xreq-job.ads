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

with Ada.Strings.Unbounded;
with Util.IO;
with XReq.Features.Handles;
with XReq.Features.Result.Handles;
with XReq.Environment;
with XReq.Environment.Handles;
with XReqLib;

use Ada.Strings.Unbounded;
use Util.IO;
use XReq.Features.Handles;
use XReq.Features.Result.Handles;
use XReq.Environment.Handles;
use XReqLib;

package XReq.Job is

   ----------------
   --  Job_Type  --
   ----------------

   --  This type describes a job to run, that is a feature that is to be
   --  compiled in a test file.

   type Job_Type is tagged private;

   function  Feature_File (Job : in Job_Type) return String;
   function  Result       (Job : in Job_Type) return Result_Feature_Handle;

   procedure Make     (Job           : out    Job_Type;
                       Feature_File  : in     String);
   procedure Run      (Job           : in out Job_Type;
                       Env           : in out Environment_Handle;
                       Logger        : in     Logger_Ptr;
                       Add_Steps_Pkg : in     String  := "";
                       Step_Matching : in     Boolean := False);
   --  If Add_Steps_Pkg is not empty, add missing step definitions to this
   --  package


   procedure Init (Env          : in out Environment_Handle;
                   Job          : in out Job_Type;
                   Logger       : in     Logger_Ptr;
                   Feature_File : in     String;
                   Step_Dir     : in     String_Vector :=
                                         Empty_String_Vector;
                   Out_Dir      : in     String := "");

private

   type Job_Type is tagged
      record
         Feature_File : Unbounded_String;
         Feature      : Feature_Handle;
         Result       : Result_Feature_Handle;
      end record;

end XReq.Job;
