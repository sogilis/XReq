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
with XReq.Steps;

use Ada.Strings.Unbounded;
use XReq.Steps;

package body XReq.Result_Scenarios is
   --------------------------------------
   --  Result_Scenario_Type  --  Make  --
   --------------------------------------

   procedure Make             (Res           : out    Result_Scenario_Type;
                               Scenario      : in     Scenario_Type) is
   begin
      Res.Make (Scenario.Name,
                Scenario.Position,
                Scenario.Outline,
                Scenario.Tag_Vector);
      if Scenario.Outline then
         Res.Set_Table (Scenario.Table);
      end if;
   end Make;

   --------------------------------------------------
   --  Result_Scenario_Type  --  Process_Scenario  --
   --------------------------------------------------

   procedure Process_Scenario (Res      : out Result_Scenario_Type;
                               Scenario : in  Scenario_Type;
                               Steps    : in  Step_File_List_Handle;
                               Log      : in  Logger_Ptr;
                               Errors   : out Boolean;
                               Missing_Steps : in out String_Set;
                               Step_Matching : in Boolean := False)
   is
      use Result_Steps;
      use Result_Steps_Vectors2;
      J         : Result_Steps.Cursor;
      Res_St    : Result_Step_Type;
      Steps_tmp : Result_Steps.Vector;
      Err       : Boolean;
   begin
      Res.Make (Scenario);
      Clear (Res.Scenarios);
      Errors := False;
      --
      --  Process all steps
      --
      for I in Scenario.Step_First .. Scenario.Step_Last loop
         if Scenario.Outline then
            Err := False;
            Res_St.Make (Scenario.Step_Element (I));
         else
            Res_St.Process_Step (Scenario.Step_Element (I),
                                 Steps,
                                 Log, Err, Step_Matching, Missing_Steps);
         end if;
         if Err then
            Errors := True;
         else
            Res.Step_Append (Res_St);
         end if;
      end loop;
      --
      --  For scenario outlines, create scenarios
      --
      if Scenario.Outline then
         for Y in Scenario.Table.First_Y + 1 .. Scenario.Table.Last_Y loop
            --
            --  For each row,
            --  take each cell and replace <Label> with the actual item in each
            --  steps
            --
            Clear (Steps_tmp);
            for I in Scenario.Step_First .. Scenario.Step_Last loop
               Res_St.Make (Scenario.Step_Element (I));
               Append (Steps_tmp, Res_St);
            end loop;
            for X in Scenario.Table.First_X .. Scenario.Table.Last_X loop
               declare
                  Item  : constant String := Scenario.Table.Item (X, Y, "");
                  Label : constant String := "<" & Scenario.Table.Item
                                   (X, Scenario.Table.First_Y, "") & ">";
               begin
                  J := First (Steps_tmp);
                  while Has_Element (J) loop
                     Res_St := Element (J);
                     --  Log.Put_Line ("Replace: " & Res_St.To_String);
                     Res_St.Set_Stanza (Replace (Res_St.Stanza, Label, Item));
                     --  Log.Put_Line ("With   : " & Res_St.To_String);
                     Replace_Element (Steps_tmp, J, Res_St);
                     Next (J);
                  end loop;
               end;
            end loop;
            --
            --  Now that we have the scenario for the row, process their steps
            --
            J := First (Steps_tmp);
            while Has_Element (J) loop
               Res_St := Element (J);
               --  Log.Put_Line ("I get  : " & Res_St.To_String);
               Process_Step (Res_St, Step_Type (Res_St),
                             Steps,
                             Log, Err, Step_Matching, Missing_Steps);
               if Err then
                  Errors := True;
               end if;
               Replace_Element (Steps_tmp, J, Res_St);
               Next (J);
            end loop;
            --  Log.Put_Line ("--------");
            --
            --  And append the scenario in the scenario outline
            --
            Append (Res.Scenarios, Steps_tmp);
         end loop;
      end if;
   end Process_Scenario;

   -------------------------------------------
   --  Result_Scenario_Type  --  To_String  --
   -------------------------------------------

   function  To_Code        (Res      : in     Result_Scenario_Type;
                               Indent   : in     String := "")
                                          return String
   is
      use Result_Steps;
      CRLF   : constant String := "" & ASCII.LF;
      Buffer : Unbounded_String;
   begin
      for I in Res.Step_First .. Res.Step_Last loop
         Append (Buffer,
                 Indent & Res.Step_Element (I).To_Code & CRLF);
      end loop;
      return To_String (Buffer);
   end To_Code;

   ---------------------
   --  Outline_First  --
   ---------------------

   function  Outline_First (S : in Result_Scenario_Type) return Natural is
      pragma Unreferenced (S);
   begin
      return 0;
   end Outline_First;

   --------------------
   --  Outline_Last  --
   --------------------

   function  Outline_Last  (S : in Result_Scenario_Type) return Integer is
   begin
      return Outline_Count (S) - 1;
   end Outline_Last;

   ---------------------
   --  Outline_Count  --
   ---------------------

   function  Outline_Count (S : in Result_Scenario_Type) return Natural is
      use Result_Steps_Vectors2;
      use Ada.Containers;
   begin
      return Integer (Length (S.Scenarios));
   end Outline_Count;

   --------------------------
   --  Outline_Step_First  --
   --------------------------

   function  Outline_Step_First   (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Natural
   is
      pragma Unreferenced (S, Outline);
   begin
      return 0;
   end Outline_Step_First;

   -------------------------
   --  Outline_Step_Last  --
   -------------------------

   function  Outline_Step_Last    (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Integer
   is
   begin
      return Outline_Step_Count (S, Outline) - 1;
   end Outline_Step_Last;

   --------------------------
   --  Outline_Step_Count  --
   --------------------------

   function  Outline_Step_Count   (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Natural
   is
      use Ada.Containers;
      use Result_Steps;
      use Result_Steps_Vectors2;
   begin
      return Integer (Length (Element (S.Scenarios, Outline)));
   end Outline_Step_Count;

   ----------------------------
   --  Outline_Step_Element  --
   ----------------------------

   function  Outline_Step_Element (S       : in Result_Scenario_Type;
                                   Outline : in Natural;
                                   Step    : in Natural)
                                   return Result_Step_Type
   is
      use Result_Steps;
      use Result_Steps_Vectors2;
      V : Result_Steps.Vector;
   begin
      V := Element (S.Scenarios, Outline);
      return Element (V, Step);
   end Outline_Step_Element;

end XReq.Result_Scenarios;
