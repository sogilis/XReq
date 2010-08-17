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
with XReq.Steps.Result;

use Ada.Strings.Unbounded;
use XReq.Steps;

package body XReq.Scenarios.Result is
   --------------------------------------
   --  Result_Scenario_Type  --  Make  --
   --------------------------------------

   procedure Make             (Res           : out    Result_Scenario_Type;
                               Scenario      : in     Scenario_Handle) is
   begin
      pragma Assert (Scenario.Valid);
      Res.Make (Scenario.R.Name,
                Scenario.R.Position,
                Scenario.R.Outline,
                Scenario.R.Tag_Vector);
      if Scenario.R.Outline then
         Res.Set_Table (Scenario.R.Table);
      end if;
   end Make;

   --------------------------------------------------
   --  Result_Scenario_Type  --  Process_Scenario  --
   --------------------------------------------------

   procedure Process_Scenario (Res      : in out Result_Scenario_Type;
                               Scenario : in  Scenario_Handle;
                               Steps    : in  Step_File_List_Handle;
                               Log      : in  Logger_Ptr;
                               Errors   : out Boolean;
                               Missing_Steps : in out String_Set;
                               Step_Matching : in Boolean := False)
   is
      package Step_Vectors is new Ada.Containers.Vectors
        (Natural, Step_Handle, "=");
      use Result_Steps;
      use Step_Vectors;
      use Result_Steps_Vectors2;
      K          : Step_Vectors.Cursor;
      Res_St     : Result_Step_Handle;
      St         : Step_Handle;
      Steps_tmp  : Result_Steps.Vector;
      Steps_tmp2 : Step_Vectors.Vector;
      Err        : Boolean;
      Counter    : Integer;
      I          : Integer;
   begin
      pragma Assert (Scenario.Valid);
      Res.Make (Scenario);
      Clear (Res.Scenarios);
      Errors := False;
      --
      --  Process all steps
      --
      for I in Scenario.R.Step_First .. Scenario.R.Step_Last loop
         Counter := Res.Step_Count;
         Res_St := Create;
         if Scenario.R.Outline then
            Err := False;
            Res_St.R.Make (Scenario.R.Step_Element (I));
         else
            Res_St.R.Process_Step
              (Scenario.R.Step_Element (I), Steps,
               Log, Err, Step_Matching, Missing_Steps);
         end if;
         if Err then
            Errors := True;
         else
            Res.Step_Append (Res_St);
            pragma Assert (Res.Step_Count = Counter + 1);
         end if;
         Res_St.UnRef;
         pragma Assert (Err or Res.Step_Count = Counter + 1);
      end loop;
      --
      --  For scenario outlines, create scenarios
      --
      if Scenario.R.Outline then
         I := Res.Outline_First;
         for Y in Scenario.R.Table.First_Y + 1 .. Scenario.R.Table.Last_Y loop
            --
            --  For each row in the examples table,
            --  take each cell and replace <Label> with the actual item in each
            --  steps
            --
            Clear (Steps_tmp);
            Clear (Steps_tmp2);
            --  First, populate the step vector with a copy of the unmodified
            --  steps
            for I in Scenario.R.Step_First .. Scenario.R.Step_Last loop
               St := Create;
               St.Set_New (Step_Type (Scenario.R.Step_Element (I).Ref.all));
               Append (Steps_tmp2, St);
               St.UnRef;
            end loop;
            --  Then, for each column, replace the <label>
            for X in Scenario.R.Table.First_X .. Scenario.R.Table.Last_X loop
               declare
                  Item  : constant String := Scenario.R.Table.Item (X, Y, "");
                  Label : constant String := "<" & Scenario.R.Table.Item
                                   (X, Scenario.R.Table.First_Y, "") & ">";
               begin
                  K := First (Steps_tmp2);
                  while Has_Element (K) loop
                     St := Element (K);
                     --  Log.Put_Line ("Replace: " & St.R.To_String);
                     St.R.Set_Stanza
                       (Replace (St.R.Stanza, Label, Item));
                     --  Log.Put_Line ("With   : " & St.R.To_String);
                     --  Not necessary anymore for we use pointers
                     --  Replace_Element (Steps_tmp, J, St);
                     Next (K);
                  end loop;
               end;
            end loop;
            --
            --  Now that we have the scenario for the row, process their steps
            --
            K := First (Steps_tmp2);
            while Has_Element (K) loop
               St := Element (K);
               --  Log.Put_Line ("I get  : " & Res_St.To_String);
               Res_St := Create;
               Res_St.R.Process_Step
                 (St, Steps,
                  Log, Err, Step_Matching, Missing_Steps);
               if Err then
                  Errors := True;
               end if;
               Append (Steps_tmp, Res_St);
               Next (K);
            end loop;
            --  Log.Put_Line ("--------");
            --
            --  And append the scenario in the scenario outline
            --
            Append (Res.Scenarios, Steps_tmp);
            pragma Assert (Res.Step_Count = Res.Outline_Step_Count (I));
            I := I + 1;
         end loop;
      end if;
      pragma Assert (Errors or Res.Step_Count = Scenario.R.Step_Count);
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
                 String'(Indent & Res.Step_Element (I).R.To_Code & CRLF));
      end loop;
      return To_String (Buffer);
   end To_Code;




   --  Collection: Outlines  --------------------------------------------------

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
                                   return Result_Step_Handle
   is
      use Result_Steps;
      use Result_Steps_Vectors2;
      V : Result_Steps.Vector;
   begin
      V := Element (S.Scenarios, Outline);
      return Element (V, Step);
   end Outline_Step_Element;



   --  Inherited Collection: Steps  -------------------------------------------

   -------------------
   --  Step_Append  --
   -------------------

   procedure Step_Append  (Scenario : in out Result_Scenario_Type;
                           Stanza   : in     Result_Step_Handle) is
      Super : constant access Scenario_Type'Class := Scenario'Access;
      S : Step_Handle;
      Counter : constant Integer := Scenario.Step_Count;
   begin
      S.Set (Step_Ptr (Stanza.Ref));
      Super.Step_Append (S);
      pragma Assert (Scenario.Step_Count = Counter + 1);
   end Step_Append;

   --------------------
   --  Step_Element  --
   --------------------

   function  Step_Element (Scenario : in     Result_Scenario_Type;
                                  Index    : in     Natural)
                                             return Result_Step_Handle
   is
      Super : constant access constant Scenario_Type'Class := Scenario'Access;
      S1 : constant Step_Handle := Super.Step_Element (Index);
   begin
      return S2 : Result_Step_Handle do
         S2.Set (Steps.Result.Result_Step_Ptr (S1.Ref));
      end return;
   end Step_Element;



end XReq.Scenarios.Result;
