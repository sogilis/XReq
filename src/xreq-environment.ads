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
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with Util.IO;
with XReq.Lang;
with XReq.Step_Definitions;
with XReqLib;
with Reffy;

use Ada.Strings.Unbounded;
use Util.IO;
use XReq.Lang;
use XReq.Step_Definitions;
use XReqLib;

package XReq.Environment is

   -----------------------
   --  Job_Environment  --
   -----------------------

   package Options_Pkg is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      "="             => Ada.Strings.Unbounded."=");

   Invalid_Environment : exception;
   Invalid_Option      : exception;

   type Job_Environment is new Reffy.Counted_Type with private;
   type Job_Environment_Ptr is access all Job_Environment'Class;

   procedure Make         (Env        : in out Job_Environment;
                           Step_Dir   : in     String_Vector :=
                                               Empty_String_Vector;
                           Out_Dir    : in     String := "";
                           Language   : in     Language_Type := Lang_Ada);
   procedure Make         (Env        : in out Job_Environment;
                           Step_Dir   : in     String;
                           Out_Dir    : in     String := "";
                           Language   : in     Language_Type := Lang_Ada);
   function  First_Step_Dir (Env      : in     Job_Environment) return String;
   function  Step_Dir     (Env        : in     Job_Environment)
                                        return String_Vector;
   function  Out_Dir      (Env        : in     Job_Environment) return String;
   procedure Fill_Missing (Env        : in out Job_Environment;
                           Feature    : in     String);
   procedure Load         (Env        : in out Job_Environment;
                           Logger     : in     Logger_Ptr;
                           Fill_Steps : in     Boolean := False);
   procedure Set_Option   (Env        : in out Job_Environment;
                           Name       : in     String;
                           Value      : in     String);
   function  Get_Option   (Env        : in     Job_Environment;
                           Name       : in     String) return String;
   function  Get_Option   (Env        : in     Job_Environment;
                           Name       : in     String;
                           Default    : in     String) return String;
   function  Has_Option   (Env        : in     Job_Environment;
                           Name       : in     String) return Boolean;
   function  Language     (Env        : in     Job_Environment)
                                        return Language_Type;
   function  Loaded       (Env        : in     Job_Environment)
                                        return Boolean;
   function  Steps        (Env        : in     Job_Environment)
                                        return Step_Definitions_Type;
   procedure Steps        (Env        : in out Job_Environment;
                           Steps      : out    Step_Definitions_Ptr);

   overriding
   procedure Finalize     (Env        : in out Job_Environment);

   --  procedure Free (X : in out Job_Environment_Ptr);

   Null_Job_Environment : constant Job_Environment;

private

   type Job_Environment is new Reffy.Counted_Type with
      record
         Step_Dir  : String_Vector;
         Out_Dir   : Unbounded_String;
         Steps     : aliased Step_Definitions_Type;
         Loaded    : Boolean := False;
         Language  : Language_Type := Lang_Ada;
         Options   : Options_Pkg.Map;
      end record;

   Null_Job_Environment : constant Job_Environment :=
      (Reffy.Counted_Type with others => <>);

end XReq.Environment;

