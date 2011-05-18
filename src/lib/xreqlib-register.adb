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

package body XReqLib.Register is

   type List_Type;
   type List_Access is access all List_Type;

   type List_Type is
      record
         Proc : Feature_Procedure := null;
         Next : List_Access := null;
      end record;

   All_Procedures : List_Access := null;

   procedure Register_Feature (Proc : Feature_Procedure) is
   begin
      All_Procedures := new List_Type'(Proc, All_Procedures);
   end Register_Feature;

   procedure Call_Features
     (Format     : in out XReqLib.Format.Format_Ptr;
      Cond       : in     XReqLib.Format.Conditional_Type;
      Report     : in out XReqLib.Report.Report_Type;
      List_Mode  : in Boolean := False;
      Count_Mode : in Boolean := False)
   is
      P : List_Access := All_Procedures;
   begin
      while P /= null loop
         P.Proc (Format, Cond, Report, List_Mode, Count_Mode);
         P := P.Next;
      end loop;
   end Call_Features;

end XReqLib.Register;
