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

package body XReqLib.String_Tables is

   function  Width   (T    : in Table;
                      X    : in Integer) return Natural
   is
      W : Natural := 0;
   begin
      for Y in T.First_Y .. T.Last_Y loop
         W := Natural'Max (W, T.Item (X, Y, "")'Length);
      end loop;
      return W;
   end Width;

   function  Item    (T    : in Table;
                      X, Y : in Integer) return String is
   begin
      return To_String (Item (T, X, Y));
   end Item;

   function  Item    (T    : in Table;
                      X, Y : in Integer;
                      Def  : in String) return String is
   begin
      return To_String (Item (T, X, Y));
   exception
      when Constraint_Error =>
         return Def;
   end Item; --  GCOV_IGNORE

   procedure Put     (T    : in out Table;
                      X, Y : in     Integer;
                      Elem : in     String) is
   begin
      Put (T, X, Y, To_Unbounded_String (Elem));
   end Put;

   function  Element     (C : in     Cursor) return String is
   begin
      return To_String (Element (C));
   end Element;

   function  Get_Record (T   : in Table;
                         Rec : in Positive;
                         Set : in String) return String
   is
   begin
      return To_String
        (T.Get_Record (T.Data_Set_For (To_Unbounded_String (Set)), Rec));
   end Get_Record;

   procedure Compare_With (T     : in Table;
                           Other : in Table;
                           Ignore_Missing_Headers : in Boolean := False)
   is
      Result   : Boolean;
      Reason   : Comparison_Failure_Type;
      DataSet1 : Table_Data_Set;
      DataSet2 : Table_Data_Set;
      Rec      : Natural;
   begin
      T.Compare (Other                  => Other,
                 Ignore_Missing_Headers => Ignore_Missing_Headers,
                 Result                 => Result,
                 Reason                 => Reason,
                 DataSet1               => DataSet1,
                 DataSet2               => DataSet2,
                 Rec                    => Rec);
      if not Result then
         case Reason is
            when Fail_Sparse =>
               raise Comparison_Failed with "Sparse table";
            when Fail_Num_Records =>
               raise Comparison_Failed with "Number of record not identical:"
                   & T.Records_Count'Img & " and" & Other.Records_Count'Img;
            when Fail_Missing_Header =>
               if DataSet1 = 0 then
                  raise Comparison_Failed with
                    "Missing Header in first table: "
                      & To_String (Other.Get_Record (DataSet2, 0));
               else
                  raise Comparison_Failed with
                    "Missing Header in second table: "
                      & To_String (T.Get_Record (DataSet1, 0));
               end if;
            when Fail_Cell =>
               raise Comparison_Failed with
                 "Record" & Rec'Img & " is not identical for data set "
                   & To_String (T.Get_Record (DataSet1, 0));
         end case;
      end if;
   end Compare_With;

end XReqLib.String_Tables;
