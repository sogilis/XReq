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

use Ada.Strings.Unbounded;

package body XReq.Result_Features is

   ------------------------------------------------
   --  Result_Feature_Type  --  Process_Feature  --
   ------------------------------------------------

   procedure Process_Feature (Res     : out Result_Feature_Type;
                              Feature : in  Generic_Feature_Ptr;
                              Steps   : in  Step_Definitions_Type;
                              Log     : in  Logger_Ptr;
                              Missing_Steps : in out String_Set;
                              Step_Matching : in     Boolean := False)
   is
      R_Scen : Result_Scenario_Type;
      Result : Result_Feature_Type;
      Errors : Boolean;
   begin
      if not Feature.Parsed then
         raise Unparsed_Feature;
      end if;
      Make (Result, Feature.Name);
      Result.Set_Position    (Feature.Position);
      Result.Set_Description (Feature.Description);
      Result.Set_Filetype    (Feature.Filetype);
      Result.Lang :=          Feature.Language;
      Process_Scenario (R_Scen, Feature.Background,
                        Steps,
                        Log, Errors, Missing_Steps, Step_Matching);
      Result.Set_Background (R_Scen);
      if Errors then
         Result.Fail := True;
      end if;
      for I in Feature.Scenario_First .. Feature.Scenario_Last loop
         Process_Scenario (R_Scen, Feature.Scenario_Element (I),
                           Steps,
                           Log, Errors, Missing_Steps, Step_Matching);
         if Errors then
            Result.Fail := True;
         end if;
         Result.Scenario_Append (R_Scen);
      end loop;
      if Result.Fail then
         Log.Put_Line ("XReq can create the procedures for you if you " &
                       "use --fill-steps");
      end if;
      Assert (Result.Language.Valid);
      Res := Result;
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
      S      : constant String := Res.Background.Name;
      E      : Result_Scenario_Type;
   begin
      Append (Buffer, Indent & "Feature " & Res.Name & CRLF);
      Append (Buffer, Indent & "   Background " & S & CRLF);
      Append (Buffer, Res.Background.To_Code (Indent & "      "));
      Append (Buffer, Indent & "   End Background " & S & CRLF);
      for I in Res.Scenario_First .. Res.Scenario_Last loop
         E := Res.Scenario_Element (I);
         Append (Buffer, Indent & "   Scenario " & E.Name & CRLF);
         Append (Buffer, E.To_Code (Indent & "      "));
         Append (Buffer, Indent & "   End Scenario " & E.Name & CRLF);
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

   ----------------
   --  Language  --
   ----------------

   function  Language (F    : in     Result_Feature_Type)
                              return Language_Handle
   is
   begin
      return F.Lang;
   end Language;

end XReq.Result_Features;
