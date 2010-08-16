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

with XReqLib;
with XReq.Scenarios;
with XReqLib.Generic_Features;
with XReq.Language.Handles;

use XReqLib;
use XReq.Scenarios;
use XReq.Language.Handles;

package XReq.Features is

   Parse_Error : exception;

   --------------------
   --  Feature_Type  --
   --------------------

   package Features_Pkg is new XReqLib.Generic_Features
      (Scenario_Type, XReq.Scenarios.Equals);

   subtype Feature_Type is Features_Pkg.Feature_Type;
   subtype Feature_Ptr  is Features_Pkg.Feature_Ptr;

   procedure Free (F : in out Feature_Ptr)  renames Features_Pkg.Free;
   procedure Make (F :    out Feature_Type;
                   Name : in  String := "") renames Features_Pkg.Make;

   Unparsed_Feature : exception renames Features_Pkg.Unparsed_Feature;
   Null_Feature     : constant Feature_Type := Features_Pkg.Null_Feature;

   ----------------------------
   --  Generic_Feature_Type  --
   ----------------------------

   type Generic_Feature_Type is new Feature_Type with private;
   type Generic_Feature_Ptr  is access all Generic_Feature_Type'Class;

   function  Language  (F : in Generic_Feature_Type) return Language_Handle;



private  ----------------------------------------------------------------------

   type Generic_Feature_Type is new Feature_Type with
      record
         Lang : Language_Handle;
      end record;

end XReq.Features;
