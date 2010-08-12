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

with XReq.Step_Definitions.Ada05;
with XReq.Step_Definitions.C;
with XReq.Step_Definition_List.Handles;
with XReqLib;

use XReqLib;

package body XReq.Step_Definition_List is

   ------------
   --  Load  --
   ------------

   procedure Load (Steps      : in out Step_File_List_Type;
                   Logger     : in     Logger_Ptr;
                   Directory  : in     String;
                   Language   : in     Language_Type;
                   Fill_Steps : in     Boolean := False)
   is
      Self : Handles.Step_File_List_Handle :=
             Handles.Handles_Pkg.Create (Steps'Unchecked_Access);
   begin
      case Language is
         when Lang_Ada =>
            Logger.Put_Line ("Load Ada steps in: " & Directory);
            XReq.Step_Definitions.Ada05.Parse_Directory
               (Self, Logger, Directory, Fill_Steps);
         when Lang_C =>
            Logger.Put_Line ("Load C steps in: " & Directory);
            XReq.Step_Definitions.C.Parse_Directory
               (Self, Logger, Directory, Fill_Steps);
      end case;
   end Load;

   ------------
   --  Load  --
   ------------

   procedure Load (Steps     : in out Step_File_List_Type;
                   Directory : in     String;
                   Language  : in     Language_Type) is
   begin
      Steps.Load (Null_Logger, Directory, Language);
   end Load;

   -----------------
   --  Add_Steps  --
   -----------------

   procedure Add_Steps (Steps      : in out Step_File_List_Type;
                        New_Steps  : in     String_Set;
                        Step_Pkg   : in     String;
                        Directory  : in     String;
                        Language   : in     Language_Type;
                        Logger     : in     Logger_Ptr)
   is
      Self : Handles.Step_File_List_Handle :=
             Handles.Handles_Pkg.Create (Steps'Unchecked_Access);
   begin
      case Language is
         when Lang_Ada =>
            XReq.Step_Definitions.Ada05.Add_Steps
               (Self, New_Steps, Step_Pkg, Directory, Logger);
         when Lang_C =>
            raise Not_Yet_Implemented;  --  GCOV_IGNORE
      end case;
   end Add_Steps;

   --------------
   --  Append  --
   --------------

   procedure Append    (Steps      : in out Step_File_List_Type;
                        File       : in     Step_File_Handle) is
   begin
      Step_Definition_Vectors.Append (Steps.List, File);
   end Append;

   -------------
   --  First  --
   -------------

   function  First     (Steps      : in  Step_File_List_Type) return Natural is
   begin
      return Step_Definition_Vectors.First_Index (Steps.List);
   end First;

   ------------
   --  Last  --
   ------------

   function  Last      (Steps      : in  Step_File_List_Type) return Integer is
   begin
      return Step_Definition_Vectors.Last_Index (Steps.List);
   end Last;

   -------------
   --  Count  --
   -------------

   function  Count     (Steps      : in  Step_File_List_Type) return Natural is
   begin
      return Steps.Last - Steps.First + 1;
   end Count;

   ---------------
   --  Element  --
   ---------------

   function  Element   (Steps      : in  Step_File_List_Type;
                        Idx        : in  Natural) return Step_File_Handle is
   begin
      return Step_Definition_Vectors.Element (Steps.List, Idx);
   end Element;

   ----------------
   --  Contains  --
   ----------------

   function  Contains  (Steps     : in Step_File_List_Type;
                        Stanza    : in Step_Type) return Boolean
   is
   begin
      return Find (Steps, Stanza) /= "";
   end Contains;

   ------------
   --  Find  --
   ------------

   function  Find      (Steps     : in Step_File_List_Type;
                        Stanza    : in Step_Type) return String
   is
      Proc    : Unbounded_String;
      Matches : Step_Match_Vectors.Vector;
      Found   : Boolean;
   begin
      Find (Steps, Stanza, Proc, Matches, Found);
      if Found then
         return To_String (Proc);
      else
         return "";
      end if;
   end Find;  --  GCOV_IGNORE

   ------------
   --  Find  --
   ------------

   function  Find      (Steps     : in Step_File_List_Type;
                        Stanza    : in Step_Type) return Step_Match_Type
   is
      use Step_Definition_Vectors;
      Result : Step_Match_Type;
      I      : Step_Definition_Vectors.Cursor := First (Steps.List);
      Step   : Step_File_Handle;
      Res2   : Step_Match_Type;
   begin
      while Has_Element (I) loop
         Step := Element (I);
         Res2 := Step.R.all.Find (Stanza);
         if Res2.Match then
            if Result.Match then
               raise Step_Definitions.Ambiguous_Match;
            else
               Result := Res2;
            end if;
         end if;
         Next (I);
      end loop;
      return Result;
   end Find;  --  GCOV_IGNORE

   ------------
   --  Find  --
   ------------

   procedure Find      (Steps     : in  Step_File_List_Type;
                        Stanza    : in  Step_Type;
                        Proc      : out Unbounded_String;
                        Matches   : out Step_Match_Vectors.Vector;
                        Found     : out Boolean)
   is
      Res : constant Step_Match_Type := Find (Steps, Stanza);
   begin
      Found := Res.Match;
      if Found then
         Proc    := Res.Proc_Name;
         Matches := Res.Matches;
      end if;
   end Find;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize (Steps : in out Step_File_List_Type) is
      use Step_Definition_Vectors;
   begin
      Clear (Steps.List);
   end Finalize;


end XReq.Step_Definition_List;

