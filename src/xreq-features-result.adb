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
with XReq.Scenarios;

use Ada.Strings.Unbounded;

package body XReq.Features.Result is

   ------------------------------------------------
   --  Result_Feature_Type  --  Process_Feature  --
   ------------------------------------------------

   procedure Process_Feature (Res           : in out Result_Feature_Type;
                              Feature       : in     Feature_Handle;
                              Steps         : in     Step_File_List_Handle;
                              Log           : in     Logger_Ptr;
                              Missing_Steps : in out String_Set;
                              Step_Matching : in     Boolean := False)
   is
      R_Scen  : Result_Scenario_Handle;
      Errors  : Boolean;
      Counter : Integer;
   begin
      if not Feature.R.Parsed then
         raise Unparsed_Feature;
      end if;
      pragma Assert (Feature.R.Background.Valid);
      Res.Set_Name        (Feature.R.Name);
      Res.Set_Position    (Feature.R.Position);
      Res.Set_Description (Feature.R.Description);
      Res.Set_Filetype    (Feature.R.Filetype);
      Res.Lang :=          Feature.R.Language;
      R_Scen := Create;
      R_Scen.Ref.Process_Scenario
        (Feature.R.Background, Steps,
         Log, Errors, Missing_Steps, Step_Matching);
      pragma Assert (R_Scen.Valid);
      Res.Set_Background (R_Scen);
      pragma Assert (Res.Background.Valid);
      if Errors then
         Res.Fail := True;
      end if;
      for I in Feature.R.Scenario_First .. Feature.R.Scenario_Last loop
         Counter := Res.Scenario_Count;
         R_Scen := Create;
         R_Scen.Ref.Process_Scenario
           (Feature.R.Scenario_Element (I), Steps,
            Log, Errors, Missing_Steps, Step_Matching);
         if Errors then
            Res.Fail := True;
         else
            Res.Scenario_Append (R_Scen);
            pragma Assert (Res.Scenario_Count = Counter + 1);
         end if;
         pragma Assert (Errors or Res.Scenario_Count = Counter + 1);
      end loop;
      if Res.Fail then
         Log.Put_Line ("XReq can create the procedures for you if you " &
                       "use --fill-steps");
      end if;
      pragma Assert (Res.Language.Valid);
      pragma Assert (Res.Background.Valid);
      pragma Assert (Res.Fail or
                     Res.Scenario_Count = Feature.R.Scenario_Count);
   end Process_Feature;

   ------------------------------------------
   --  Result_Feature_Type  --  To_String  --
   ------------------------------------------

   function  To_Code        (Res      : in     Result_Feature_Type;
                             Indent   : in     String := "")
                                        return String
   is
      CRLF   : constant String := "" & ASCII.LF;
      Buffer : Unbounded_String;
      S      : constant String := Res.Background.R.Name;
      E      : Result_Scenario_Handle;
   begin
      Append (Buffer, Indent & "Feature " & Res.Name & CRLF);
      Append (Buffer, Indent & "   Background " & S & CRLF);
      Append (Buffer, Background (Res).R.To_Code (Indent & "      "));
      Append (Buffer, Indent & "   End Background " & S & CRLF);
      for I in Res.Scenario_First .. Res.Scenario_Last loop
         E := Res.Scenario_Element (I);
         Append (Buffer, Indent & "   Scenario " & E.R.Name & CRLF);
         Append (Buffer, E.R.To_Code (Indent & "      "));
         Append (Buffer, Indent & "   End Scenario " & E.R.Name & CRLF);
      end loop;
      Append (Buffer, Indent & "End Feature " & Res.Name & CRLF);
      return To_String (Buffer);
   end To_Code;

   ------------
   --  Fail  --
   ------------

   function  Fail (F : in Result_Feature_Type) return Boolean is
   begin
      return F.Fail;
   end Fail;

   ----------------
   --  Set_Fail  --
   ----------------

   procedure Set_Fail (F    : in out Result_Feature_Type;
                       Fail : in     Boolean := True) is
   begin
      F.Fail := Fail;
   end Set_Fail;

   ------------------
   --  Background  --
   ------------------

   function  Background     (F    : in     Result_Feature_Type)
                                    return Result_Scenario_Handle is
      Super : constant access constant Feature_Type'Class := F'Access;
      S1    : constant Scenario_Handle := Super.Background;
   begin
      return S2 : Result_Scenario_Handle do
         S2.Set (Scenarios.Result.Result_Scenario_Ptr (S1.Ref));
      end return;
   end Background;

   ----------------------
   --  Set_Background  --
   ----------------------

   procedure Set_Background (F    : in out Result_Feature_Type;
                             Bg   : in     Result_Scenario_Handle) is
      use XReq.Scenarios;
      Ptr   : Scenario_Ptr;
   begin
      Ptr := Scenario_Ptr (Bg.Ref);
      if Bg.Valid then
         pragma Assert (Ptr /= null);
         null;
      else
         pragma Assert (Ptr  = null);
         null;
      end if;
      F.Background.Set (Ptr);
      pragma Assert (F.Background.Valid = Bg.Valid);
   end Set_Background;

   --  Inbherited Collection: Scenario  ---------------------------------------

   ------------------------
   --  Scenario_Element  --
   ------------------------

   function  Scenario_Element   (F : in     Result_Feature_Type;
                                 I : in     Natural)
                                     return Result_Scenario_Handle is
      Super : constant access constant Feature_Type'Class := F'Access;
      S1 : constant Scenario_Handle := Super.Scenario_Element (I);
   begin
      return S2 : Result_Scenario_Handle do
         S2.Set (Scenarios.Result.Result_Scenario_Ptr (S1.Ref));
      end return;
   end Scenario_Element;

   -----------------------
   --  Scenario_Append  --
   -----------------------

   procedure Scenario_Append    (F : in out Result_Feature_Type;
                                 S : in     Result_Scenario_Handle) is
      Counter : constant Integer := F.Scenario_Count;
      Super : constant access Feature_Type'Class := F'Access;
      S1 : Scenario_Handle;
   begin
      S1.Set (Scenarios.Scenario_Ptr (S.Ref));
      Super.Scenario_Append (S1);
      pragma Assert (F.Scenario_Count = Counter + 1);
   end Scenario_Append;
end XReq.Features.Result;
