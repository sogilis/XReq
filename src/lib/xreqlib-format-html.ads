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

with XReqLib.Format.Base; use XReqLib.Format.Base;
with Ada.Containers.Vectors;

package XReqLib.Format.HTML is

   type HTML_Format_Type is new Base_Format_Type with private;
   type HTML_Format_Ptr  is access all HTML_Format_Type;

   ----------------------------------------------------------------------------

   overriding
   procedure Begin_Tests      (Format     : in out HTML_Format_Type);
   overriding
   procedure End_Tests        (Format     : in out HTML_Format_Type);
   overriding
   procedure Put_Summary      (Format     : in out HTML_Format_Type;
                               Report     : in     Report_Type;
                               D          : in     Duration);

   ----------------------------------------------------------------------------

   overriding
   procedure Put_Feature      (Format     : in out HTML_Format_Type);
   overriding
   procedure End_Feature      (Format     : in out HTML_Format_Type);

   ----------------------------------------------------------------------------

   overriding
   procedure Enter_Outline    (Format     : in out HTML_Format_Type);
   overriding
   procedure Begin_Outline    (Format     : in out HTML_Format_Type);
   overriding
   procedure Put_Outline_Report
                              (Format     : in out HTML_Format_Type;
                               Table      : in     Table_Type);

   ----------------------------------------------------------------------------

   overriding
   procedure Enter_Scenario   (Format     : in out HTML_Format_Type);
   overriding
   procedure Begin_Scenario   (Format     : in out HTML_Format_Type);
   overriding
   procedure End_Scenario     (Format     : in out HTML_Format_Type);

   ----------------------------------------------------------------------------

   overriding
   procedure Begin_Background (Format   : in out HTML_Format_Type);
   overriding
   procedure End_Background   (Format   : in out HTML_Format_Type);

   ----------------------------------------------------------------------------

   overriding
   procedure Put_Step       (Format     : in out HTML_Format_Type;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type);
   overriding
   procedure Put_Error      (Format     : in out HTML_Format_Type;
                             Err        : in     Exception_Occurrence);
   overriding
   procedure End_Step       (Format     : in out HTML_Format_Type);

   ----------------------------------------------------------------------------

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

   type HTML_Format_Type is new Base_Format_Type with
      record
         Has_Debug       : Boolean := False;
         Curr_Scenario   : Menu_Item_2;
         Curr_Feature    : Menu_Item_1;
         Menu            : Menu_Vectors.Vector;
      end record;


end XReqLib.Format.HTML;
