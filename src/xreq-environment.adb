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

with Ada.Directories;
with Ada.Containers;
--  with Ada.Unchecked_Deallocation;

use Ada.Directories;

package body XReq.Environment is

   ---------------------------------
   --  Job_Environment  -- Make   --
   ---------------------------------

   procedure Make         (Env      : in out Job_Environment;
                           Step_Dir : in     String_Vector :=
                                             Empty_String_Vector;
                           Out_Dir  : in     String := "";
                           Language : in     Language_Type := Lang_Ada) is
   begin
      Env := (Reffy.Counted_Type with
         Step_Dir => Step_Dir,
         Out_Dir  => To_Unbounded_String (Out_Dir),
         Language => Language,
         Options  => Env.Options,
         others   => <>);
   end Make;

   ---------------------------------
   --  Job_Environment  -- Make   --
   ---------------------------------

   procedure Make         (Env      : in out Job_Environment;
                           Step_Dir : in     String;
                           Out_Dir  : in     String := "";
                           Language : in     Language_Type := Lang_Ada)
   is
      use String_Vectors;
      V : String_Vector;
   begin
      Append (V, To_Unbounded_String (Step_Dir));
      Make (Env, V, Out_Dir, Language);
   end Make;

   -------------------------------------------
   --  Job_Environment  --  First_Step_Dir  --
   -------------------------------------------

   function  First_Step_Dir (Env : in Job_Environment) return String is
      use String_Vectors;
      use Ada.Containers;
   begin
      if Length (Env.Step_Dir) >= 1 then
         return To_String (First_Element (Env.Step_Dir));
      else
         raise Constraint_Error with "No step dir";
      end if;
   end First_Step_Dir;

   -------------------------------------
   --  Job_Environment  --  Step_Dir  --
   -------------------------------------

   function  Step_Dir     (Env        : in     Job_Environment)
                                        return String_Vector is
   begin
      return Env.Step_Dir;
   end Step_Dir;

   ------------------------------------
   --  Job_Environment  --  Out_Dir  --
   ------------------------------------

   function  Out_Dir      (Env      : in     Job_Environment) return String is
   begin
      return To_String (Env.Out_Dir);
   end Out_Dir;

   -----------------------------------------
   --  Job_Environment  --  Fill_Missing  --
   -----------------------------------------

   procedure Fill_Missing (Env : in out Job_Environment;
                           Feature : in String)
   is
      use String_Vectors;
   begin

      if Is_Empty (Env.Step_Dir) then
         Append (Env.Step_Dir, To_Unbounded_String (Compose (
            Containing_Directory (Feature), "step_definitions")));
      end if;

      if Length (Env.Out_Dir) = 0 then
         Env.Out_Dir  := To_Unbounded_String (Compose (
            Containing_Directory (Feature), "tests"));
      end if;

   end Fill_Missing;

   ---------------------------------
   --  Job_Environment  --  Load  --
   ---------------------------------

   procedure Load (Env        : in out Job_Environment;
                   Logger     : in     Logger_Ptr;
                   Fill_Steps : in     Boolean := False)
   is
      use String_Vectors;
   begin

      if Is_Empty (Env.Step_Dir) then
         raise Invalid_Environment with "No step_definitions directory";
      end if;
      if Length (Env.Out_Dir) = 0 then
         if not Fill_Steps then
            raise Invalid_Environment with "No output directory";
         end if;
      else
         Create_Path (Out_Dir (Env));
      end if;

      for I in First_Index (Env.Step_Dir) .. Last_Index (Env.Step_Dir) loop
         declare
            Step : constant String := To_String (Element (Env.Step_Dir, I));
         begin
            Create_Path (Step);
            Env.Steps.Ref.Load (Logger, Step, Env.Language, Fill_Steps);
         end;
      end loop;

      Env.Loaded := True;

   end Load;

   ---------------------------------------
   --  Job_Environment  --  Set_Option  --
   ---------------------------------------

   procedure Set_Option   (Env        : in out Job_Environment;
                           Name       : in     String;
                           Value      : in     String)
   is
      use Options_Pkg;
   begin
      Include (Env.Options,
               To_Unbounded_String (Name),
               To_Unbounded_String (Value));
   end Set_Option;

   ---------------------------------------
   --  Job_Environment  --  Get_Option  --
   ---------------------------------------

   function  Get_Option   (Env        : in     Job_Environment;
                           Name       : in     String) return String
   is
      use Options_Pkg;
      I : Cursor;
   begin
      I := Find (Env.Options, To_Unbounded_String (Name));
      if Has_Element (I) then
         return To_String (Element (I));
      else
         raise Invalid_Option;
      end if;
   end Get_Option;

   ---------------------------------------
   --  Job_Environment  --  Get_Option  --
   ---------------------------------------

   function  Get_Option   (Env        : in     Job_Environment;
                           Name       : in     String;
                           Default    : in     String) return String
   is
      use Options_Pkg;
      I : Cursor;
   begin
      I := Find (Env.Options, To_Unbounded_String (Name));
      if Has_Element (I) then
         return To_String (Element (I));
      else
         return Default;
      end if;
   end Get_Option;  --  GCOV_IGNORE

   ---------------------------------------
   --  Job_Environment  --  Has_Option  --
   ---------------------------------------

   function  Has_Option   (Env        : in     Job_Environment;
                           Name       : in     String) return Boolean
   is
      use Options_Pkg;
   begin
      return Has_Element (Find (Env.Options, To_Unbounded_String (Name)));
   end Has_Option;

   ----------------
   --  Language  --
   ----------------

   function  Language     (Env        : in     Job_Environment)
                                        return Language_Type is
   begin
      return Env.Language;
   end Language;

   --------------
   --  Loade  --
   --------------

   function  Loaded       (Env        : in     Job_Environment)
                                        return Boolean is
   begin
      return Env.Loaded;
   end Loaded;

   -------------
   --  Steps  --
   -------------

   function  Steps        (Env        : in     Job_Environment)
                                        return Step_File_List_Handle is
   begin
      return Env.Steps;
   end Steps;

end XReq.Environment;

