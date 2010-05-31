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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package XReqLib.Format.HTML is

   type HTML_Format_Type is new Format_Type with private;
   type HTML_Format_Ptr  is access all HTML_Format_Type;

   overriding
   procedure Start_Tests    (Format     : in out HTML_Format_Type);
   overriding
   procedure Stop_Tests     (Format     : in out HTML_Format_Type);

   overriding
   procedure Start_Feature  (Format     : in out HTML_Format_Type);
   overriding
   procedure Stop_Feature   (Format     : in out HTML_Format_Type);

   overriding
   procedure Start_Background (Format   : in out HTML_Format_Type;
                               First    : in Boolean);
   overriding
   procedure Stop_Background  (Format   : in out HTML_Format_Type;
                               First    : in Boolean);

   overriding
   procedure Start_Step     (Format     : in out HTML_Format_Type);
   overriding
   procedure Stop_Step      (Format     : in out HTML_Format_Type);


   overriding
   procedure Enter_Outline  (Format     : in out HTML_Format_Type);
   overriding
   procedure Start_Outline  (Format     : in out HTML_Format_Type);
   overriding
   procedure Stop_Outline   (Format     : in out HTML_Format_Type);


   overriding
   procedure Enter_Scenario (Format     : in out HTML_Format_Type);
   overriding
   procedure Start_Scenario (Format     : in out HTML_Format_Type);
   overriding
   procedure Stop_Scenario  (Format     : in out HTML_Format_Type);

   overriding
   procedure Put_Feature    (Format     : in out HTML_Format_Type;
                             Feature    : in     String;
                             Description : in    String;
                             Position   : in     String);
   overriding
   procedure Put_Background (Format     : in out HTML_Format_Type;
                             Background : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   overriding
   procedure Put_Outline    (Format     : in out HTML_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   overriding
   procedure Put_Outline_Report
                            (Format     : in out HTML_Format_Type;
                             Table      : in     Table_Type);
   overriding
   procedure Put_Scenario   (Format     : in out HTML_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   overriding
   procedure Put_Scenario_Outline
                            (Format     : in out HTML_Format_Type;
                             Num        : in     Natural;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   overriding
   procedure Put_Step       (Format     : in out HTML_Format_Type;
                             Step       : in     Step_Kind;
                             Name       : in     String;
                             Position   : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type);
   overriding
   procedure Put_Error      (Format     : in out HTML_Format_Type;
                             Err        : in     Exception_Occurrence);
   overriding
   procedure Put_Summary    (Format     : in out HTML_Format_Type;
                             Report     : in     Report_Type;
                             D          : in     Duration);

   function  New_HTML_Format return HTML_Format_Ptr;

private

   type Menu_Item_2 is
      record
         Name            : Unbounded_String;
         Status          : Status_Type := Status_Passed;
         Is_Background   : Boolean;
         Feature_ID      : Natural := 0;
         Scenario_ID     : Natural := 0;
      end record;

   package Menu_Vectors_2 is
      new Ada.Containers.Vectors (Positive, Menu_Item_2, "=");

   type Menu_Item_1 is
      record
         Name            : Unbounded_String;
         Status          : Status_Type := Status_Passed;
         Feature_ID      : Natural := 0;
         Sub_Menu        : Menu_Vectors_2.Vector;
      end record;

   package Menu_Vectors is
      new Ada.Containers.Vectors (Positive, Menu_Item_1, "=");

   type HTML_Format_Type is new Format_Type with
      record
         Close_Step      : Boolean := False;
         Have_Background : Boolean := False;
         Skip_Scenarios  : Boolean := False;
         Inline_Backgrnd : Boolean := False;
         Run_Feature     : Boolean := False;
         Has_Debug       : Boolean := False;
         Curr_Scenario   : Menu_Item_2;
         Curr_Feature    : Menu_Item_1;
         Menu            : Menu_Vectors.Vector;
      end record;


end XReqLib.Format.HTML;
