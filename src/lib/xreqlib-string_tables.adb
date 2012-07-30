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

   -----------------------
   --  Set_Header_Name  --
   -----------------------

   procedure Set_Header_Name (T : in out Table;
                              Old_Header, New_Header : String) is
   begin
      T.Set_Header_Name (To_Unbounded_String (Old_Header),
                         To_Unbounded_String (New_Header));
   end Set_Header_Name;

   -----------------------
   --  Import_Data_Set  --
   -----------------------

   procedure Import_Data_Set (T : in out Table;
                              Other_Table : in Table;
                              Other_Header : String;
                              Rename : String)
   is
   begin
      T.Import_Data_Set (Other_Table,
                         To_Unbounded_String (Other_Header),
                         To_Unbounded_String (Rename));
   end Import_Data_Set;

   --------------------
   --  Add_Data_Set  --
   --------------------

   procedure Add_Data_Set    (T : in out Table;
                              E : in     String) is
   begin
      if T.Is_Empty then
         T.Put (0, 0, "#");
      end if;
      if T.Header_Kind = None then
         T.Set_Header_Kind (First_Row);
      end if;

      T.Set_Record (T.Next_Data_Set, 0, To_Unbounded_String (E));
   end Add_Data_Set;

   ------------------
   --  Add_Record  --
   ------------------

   procedure Add_Record      (T : in out Table) is
      Rec : constant Integer := T.Records_Count + 1;
      Tmp : constant String  := Rec'Img;
      Str : constant String  := Tmp (Tmp'First + 1 .. Tmp'Last);
   begin
      T.Set_Record (1, Rec, To_Unbounded_String (Str));
   end Add_Record;

   ----------------
   --  Add_Data  --
   ----------------

   procedure Add_Data        (T : in out Table;
                              Data_Set  : in Table_Data_Set;
                              Data      : in String;
                              Auto_Next : in Boolean := True);

   procedure Add_Data        (T : in out Table;
                              Data_Set  : in Table_Data_Set;
                              Data      : in String;
                              Auto_Next : in Boolean := True)
   is
      Rec : Integer;
      Foo : Unbounded_String;
      Has_Elem : Boolean;
   begin
      Rec := T.Records_Count;

      if Auto_Next then
         T.Get_Record (Data_Set, Rec, Foo, Has_Elem);
         if Has_Elem then
            T.Add_Record;
            Rec := T.Records_Count;
         end if;
      end if;

      T.Set_Record (Data_Set, Rec, To_Unbounded_String (Data));
   end Add_Data;

   ----------------
   --  Add_Data  --
   ----------------

   procedure Add_Data        (T : in out Table;
                              Data_Set  : in String;
                              Data      : in String;
                              Auto_Next : in Boolean := True)
   is
      DS  : Table_Data_Set;
   begin
      DS  := T.Data_Set_For (To_Unbounded_String (Data_Set));
      Add_Data (T, Integer (DS), Data, Auto_Next);
   end Add_Data;

   ----------------
   --  Add_Data  --
   ----------------

   procedure Add_Data        (T : in out Table;
                              Index     : in Integer;
                              Data      : in String;
                              Auto_Next : in Boolean := True)
   is
   begin
      Add_Data (T,
                Data_Set  => Table_Data_Set (Index + 1),
                Data      => Data,
                Auto_Next => Auto_Next);
   end Add_Data;

   ----------------------
   --  Sort_Data_Sets  --
   ----------------------

   procedure Sort_Data_Sets  (T : in out Table; Key : Table_Data_Set) is
      procedure Swap (T : in out Table; Rec1, Rec2 : Integer);
      --  Swap two records

      procedure Swap (T : in out Table; Rec1, Rec2 : Integer) is
         X, Y : Unbounded_String;
      begin
         for DS in T.First_Data_Set .. T.Last_Data_Set loop
            X := T.Get_Record (DS, Rec1, Null_Unbounded_String);
            Y := T.Get_Record (DS, Rec2, Null_Unbounded_String);

            if T.Has_Record (DS, Rec1) then
               T.Set_Record (DS, Rec2, X);
            else
               T.Remove_Record (DS, Rec2);
            end if;

            if T.Has_Record (DS, Rec2) then
               T.Set_Record (DS, Rec1, Y);
            else
               T.Remove_Record (DS, Rec1);
            end if;
         end loop;
      end Swap;

      Rec_Min : constant Integer := T.First_Record;
      Rec_Max : constant Integer := T.Last_Record;
      Rec_Low : Integer := Rec_Min;
      Swapped : Boolean := True;
      A, B    : Unbounded_String;

   begin
      while Swapped loop
         Swapped := False;
         for Rec in Rec_Low .. Rec_Max - 1 loop
            A := T.Get_Record (Key, Rec,     Null_Unbounded_String);
            B := T.Get_Record (Key, Rec + 1, Null_Unbounded_String);
            if A > B then
               Swap (T, Rec, Rec + 1);
               Swapped := True;
            end if;
         end loop;
         Rec_Low := Rec_Low + 1;
      end loop;
   end Sort_Data_Sets;

   procedure Sort_Data_Sets  (T : in out Table; Key : String) is
   begin
      Sort_Data_Sets (T, Data_Set_For (T, Key));
   end Sort_Data_Sets;

   --------------------
   --  Data_Set_For  --
   --------------------

   function  Data_Set_For    (T : Table; DS : String) return Table_Data_Set is
   begin
      return T.Data_Set_For (To_Unbounded_String (DS));
   end Data_Set_For;

end XReqLib.String_Tables;
