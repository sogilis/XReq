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

package body XReqLib.Tables is

   --  GCOV_IGNORE_BEGIN
   --  Cannot cover generic package

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : in Key_Type) return Boolean is
   begin
      return (Left.X < Right.X) or (Left.X = Right.X and Left.Y < Right.Y);
   end "<";

   ------------
   -- Length --
   ------------

   function Length (T    : in Table) return Key_Type is
   begin
      return Key_Type'(T.Length_X, T.Length_Y);
   end Length;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (T    : in Table) return Boolean is
   begin
      return T.Count = 0;
   end Is_Empty;

   -----------
   -- Count --
   -----------

   function Count (T    : in Table) return Natural is
      use Maps;
   begin
      return Natural (Length (T.Map));
   end Count;

   -------------
   -- First_X --
   -------------

   function First_X (T    : in Table) return Integer is
   begin
      return T.First_X;
   end First_X;

   -------------
   -- First_Y --
   -------------

   function First_Y (T    : in Table) return Integer is
   begin
      return T.First_Y;
   end First_Y;

   ------------
   -- Last_X --
   ------------

   function Last_X (T    : in Table) return Integer is
   begin
      return T.Last_X;
   end Last_X;

   ------------
   -- Last_Y --
   ------------

   function Last_Y (T    : in Table) return Integer is
   begin
      return T.Last_Y;
   end Last_Y;

   --------------
   -- Length_X --
   --------------

   function Length_X (T    : in Table) return Integer is
   begin
      return T.Last_X - T.First_X + 1;
   end Length_X;

   --------------
   -- Length_Y --
   --------------

   function Length_Y (T    : in Table) return Integer is
   begin
      return T.Last_Y - T.First_Y + 1;
   end Length_Y;

   ----------
   -- Item --
   ----------

   function Item
     (T    : in Table;
      X, Y : in Integer)
      return Element_Type
   is
      use Maps;
   begin
      return Element (T.Map, Key_Type'(X, Y));
   exception
      when Constraint_Error =>
         raise Constraint_Error with "Not in table:" & X'Img & "," & Y'Img;
   end Item;

   ----------------------------
   --  Recompute_Boundaries  --
   ----------------------------

   procedure Recompute_Boundaries (T    : in out Table) is
      I : Cursor;
      K : Key_Type;
      Min, Max : Key_Type;
   begin
      if T.Is_Empty then
         T := Table'(others => <>);
      else
         Min := (Integer'Last, Integer'Last);
         Max := (Integer'First, Integer'First);
         I := First (T);
         while Has_Element (I) loop
            K := Key (I);
            if K.X < Min.X then Min.X := K.X; end if;
            if K.Y < Min.Y then Min.Y := K.Y; end if;
            if K.X > Max.X then Max.X := K.X; end if;
            if K.Y > Max.Y then Max.Y := K.Y; end if;
            Next (I);
         end loop;
         T.First_X := Min.X;
         T.First_Y := Min.Y;
         T.Last_X  := Max.X;
         T.Last_Y  := Max.Y;
      end if;
   end Recompute_Boundaries;

   --------------
   --  Remove  --
   --------------

   procedure Remove  (T    : in out Table;
                      X, Y : in     Integer;
                      Recompute : in Boolean := True)
   is
      use Maps;
   begin
      Exclude (T.Map, Key_Type'(X, Y));
      if Recompute then
         T.Recompute_Boundaries;
      end if;
   end Remove;

   -----------
   -- Clear --
   -----------

   procedure Clear (T    : in out Table) is
      use Maps;
   begin
      Clear (T.Map);
      T.Last_X := T.First_X - 1;
      T.Last_Y := T.First_Y - 1;
   end Clear;

   ---------
   -- Put --
   ---------

   procedure Put
     (T    : in out Table;
      X, Y : in     Integer;
      Elem : in     Element_Type)
   is
      use Maps;
   begin
      if T.First_X > T.Last_X then
         T.First_X := X;
         T.Last_X := X;
      end if;
      if T.First_Y > T.Last_Y then
         T.First_Y := Y;
         T.Last_Y := Y;
      end if;
      Include (T.Map, Key_Type'(X, Y), Elem);
      if X < T.First_X then
         T.First_X := X;
      end if;
      if X > T.Last_X then
         T.Last_X := X;
      end if;
      if Y < T.First_Y then
         T.First_Y := Y;
      end if;
      if Y > T.Last_Y then
         T.Last_Y := Y;
      end if;
   end Put;

   ----------
   -- Item --
   ----------

   procedure Item
     (T    : in     Table;
      X, Y : in     Integer;
      Elem : out    Element_Type;
      Ok   : out    Boolean)
   is
   begin
      Elem := T.Item (X, Y);
      Ok := True;
   exception
      when Constraint_Error =>
         Ok   := False;
         --  Elem := Elem;
   end Item;

   ----------
   -- Item --
   ----------

   function  Item    (T    : in     Table;
                      X, Y : in     Integer;
                      Default : in  Element_Type)
                      return Element_Type is
   begin
      return T.Item (X, Y);
   exception
      when Constraint_Error =>
         return Default;
   end Item;

   ----------------
   --  Has_Item  --
   ----------------

   function Has_Item
     (T       : in Table;
      X, Y    : in Integer)
      return Boolean
   is
      use Maps;
   begin
      return Contains (T.Map, Key_Type'(X, Y));
   end Has_Item;

   -------------
   -- First_X --
   -------------

   procedure First_X (T    : in out Table; X : in Integer) is
   begin
      if T.Last_X < T.First_X then
         T.Last_X := X - 1;
      end if;
      T.First_X := X;
   end First_X;

   -------------
   -- First_Y --
   -------------

   procedure First_Y (T    : in out Table; Y : in Integer) is
   begin
      if T.Last_Y < T.First_Y then
         T.Last_Y := Y - 1;
      end if;
      T.First_Y := Y;
   end First_Y;

   ------------
   -- Last_X --
   ------------

   procedure Last_X (T    : in out Table; X : in Integer) is
   begin
      T.Last_X := X;
      if T.Last_X < T.First_X then
         T.Last_X := T.First_X - 1;
      end if;
   end Last_X;

   ------------
   -- Last_Y --
   ------------

   procedure Last_Y (T    : in out Table; X : in Integer) is
   begin
      T.Last_Y := X;
      if T.Last_Y < T.First_Y then
         T.Last_Y := T.First_Y - 1;
      end if;
   end Last_Y;

   -----------
   -- Add_X --
   -----------

   procedure Add_X (T    : in out Table) is
   begin
      T.Last_X := T.Last_X + 1;
   end Add_X;

   -----------
   -- Add_Y --
   -----------

   procedure Add_Y (T    : in out Table) is
   begin
      T.Last_Y := T.Last_Y + 1;
   end Add_Y;

   -------------
   --  First  --
   -------------

   function  First       (T : in     Table) return Cursor is
   begin
      return Cursor'(C => Maps.First (T.Map));
   end First;

   ------------
   --  Next  --
   ------------

   procedure Next        (C : in out Cursor) is
   begin
      Maps.Next (C.C);
   end Next;

   ---------------
   --  Element  --
   ---------------

   function  Element     (C : in     Cursor) return Element_Type is
   begin
      return Maps.Element (C.C);
   end Element;

   -----------
   --  Key  --
   -----------

   function  Key         (C : in     Cursor) return Key_Type is
   begin
      return Maps.Key (C.C);
   end Key;

   -------------------
   --  Has_Element  --
   -------------------

   function  Has_Element (C : in     Cursor) return Boolean is
   begin
      return Maps.Has_Element (C.C);
   end Has_Element;

   -------------------
   --  Header_Kind  --
   -------------------

   function  Header_Kind     (T : in     Table) return Table_Header_Kind is
   begin
      return T.Head;
   end Header_Kind;

   -----------------------
   --  Set_Header_Kind  --
   -----------------------

   procedure Set_Header_Kind (T : in out Table;
                              H : in     Table_Header_Kind := None) is
   begin
      T.Head := H;
   end Set_Header_Kind;

   ------------------
   --  Data_To_XY  --
   ------------------

   procedure Data_To_XY      (T    : in     Table;
                              DS   : in     Table_Data_Set;
                              Rec  : in     Integer;
                              X    : out    Integer;
                              Y    : out    Integer)
   is
      Rec_Offset : Integer;
   begin
      if T.Head = Transpose or T.Head = None then
         --  No header, the first data at index 1 must be brought back to 0
         Rec_Offset := 1;
      else
         --  If there is a header, it is at index 0
         Rec_Offset := 0;
      end if;
      if T.Head = Transpose or T.Head = First_Column then
         --  Data Sets are rows (the first column may be the header)
         X := T.First_X + Rec          - Rec_Offset;
         Y := T.First_Y + Integer (DS) - 1;
      else
         --  Data Sets are columns (the first row may be the header)
         X := T.First_X + Integer (DS) - 1;
         Y := T.First_Y + Rec          - Rec_Offset;
      end if;
   end Data_To_XY;

   ------------------
   --  XY_To_Data  --
   ------------------

   procedure XY_To_Data      (T    : in     Table;
                              X    : in     Integer;
                              Y    : in     Integer;
                              DS   : out    Table_Data_Set;
                              Rec  : out    Integer)
   is
      Rec_Offset : Integer;
   begin
      if T.Head = Transpose or T.Head = None then
         --  No header, the first data at index 1 must be brought back to 0
         Rec_Offset := 1;
      else
         --  If there is a header, it is at index 0
         Rec_Offset := 0;
      end if;
      if T.Head = Transpose or T.Head = First_Column then
         --  Data Sets are rows (the first column may be the header)
         Rec := Integer        (X - T.First_X + Rec_Offset);
         DS  := Table_Data_Set (Y - T.First_Y + 1);
      else
         --  Data Sets are columns (the first row may be the header)
         DS  := Table_Data_Set (X - T.First_X + 1);
         Rec := Integer        (Y - T.First_Y + Rec_Offset);
      end if;
   end XY_To_Data;

   -----------------------
   --  Data_Sets_Count  --
   -----------------------

   function  Data_Sets_Count (T : in    Table) return Natural is
   begin
      if T.Head = Transpose or T.Head = First_Column then
         return T.Length_Y;
      else
         return T.Length_X;
      end if;
   end Data_Sets_Count;

   ---------------------
   --  First_Data_Set  --
   ---------------------

   function  First_Data_Set   (T : in     Table) return Table_Data_Set is
      pragma Unreferenced (T);
   begin
      return Table_Data_Set (1);
   end First_Data_Set;

   ---------------------
   --  Last_Data_Set  --
   ---------------------

   function  Last_Data_Set   (T : in     Table) return Table_Data_Set is
   begin
      return Table_Data_Set (T.Data_Sets_Count);
   end Last_Data_Set;

   ---------------------
   --  Next_Data_Set  --
   ---------------------

   function  Next_Data_Set   (T : in     Table) return Table_Data_Set is
   begin
      return Table_Data_Set (T.Data_Sets_Count + 1);
   end Next_Data_Set;

   ---------------------
   --  Data_Sets_For  --
   ---------------------

   function  Data_Set_For    (T : in    Table;
                              H : in    Element_Type) return Table_Data_Set
   is
      D : Table_Data_Set := 1;
   begin
      loop
         if T.Get_Record (D, 0) = H then
            return D;
         end if;
         D := D + 1;
      end loop;
   end Data_Set_For;

   ---------------------
   --  Records_Count  --
   ---------------------

   function  Records_Count   (T : in    Table) return Natural is
      Length : Integer;
   begin
      if T.Head = Transpose or T.Head = First_Column then
         Length := T.Length_X;
      else
         Length := T.Length_Y;
      end if;
      if T.Head = First_Column or T.Head = First_Row then
         Length := Length - 1;
      end if;
      return Length;
   end Records_Count;

   -------------------
   --  Head_Record  --
   -------------------

   function  Head_Record     (T : in Table) return Natural is
   begin
      if T.Header_Kind = First_Column or T.Header_Kind = First_Row then
         raise Constraint_Error with "No Header";
      else
         return 0;
      end if;
   end Head_Record;

   --------------------
   --  First_Record  --
   --------------------

   function  First_Record    (T : in Table) return Positive is
      pragma Unreferenced (T);
   begin
      return 1;
   end First_Record;

   -------------------
   --  Last_Record  --
   -------------------

   function  Last_Record     (T : in Table) return Natural is
   begin
      return T.Records_Count;
   end Last_Record;

   -------------------
   --  Next_Record  --
   -------------------

   function  Next_Record     (T : in Table) return Positive is
   begin
      return T.Last_Record + 1;
   end Next_Record;

   ------------------
   --  Get_Record  --
   ------------------

   procedure Get_Record      (T : in     Table;
                              D : in     Table_Data_Set;
                              R : in     Natural;
                              Elem : out Element_Type;
                              Ok   : out Boolean)
   is
      X, Y : Integer;
   begin
      T.Data_To_XY (D, R, X, Y);
      T.Item (X, Y, Elem, Ok);
   end Get_Record;

   ------------------
   --  Get_Record  --
   ------------------

   function  Get_Record      (T : in    Table;
                              D : in    Table_Data_Set;
                              R : in    Natural) return Element_Type
   is
      X, Y : Integer;
   begin
      T.Data_To_XY (D, R, X, Y);
      return T.Item (X, Y);
   exception
      when Constraint_Error =>
         raise Constraint_Error with "Record" & R'Img & " not in data set"
           & D'Img & ". " & T.Header_Kind'Img & T.Records_Count'Img
           & " x" & T.Last_Data_Set'Img;
   end Get_Record;

   ------------------
   --  Get_Record  --
   ------------------

   function  Get_Record
     (T : in     Table;
      D : in     Table_Data_Set;
      R : in     Natural;
      Default : in Element_Type)
      return Element_Type
   is
      Ok : Boolean;
   begin
      return E : Element_Type do
         T.Get_Record (D, R, E, Ok);
         if not Ok then
            E := Default;
         end if;
      end return;
   end Get_Record;

   ------------------
   --  Has_Record  --
   ------------------

   function Has_Record
     (T    : in  Table;
      D    : in  Table_Data_Set;
      R    : in  Natural)
      return Boolean
   is
      X, Y : Integer;
   begin
      T.Data_To_XY (D, R, X, Y);
      return T.Has_Item (X, Y);
   end Has_Record;

   ---------------------
   --  Remove_Record  --
   ---------------------

   procedure Remove_Record
     (T    : in out Table;
      D    : in     Table_Data_Set;
      R    : in     Natural)
   is
      X, Y : Integer;
   begin
      T.Data_To_XY (D, R, X, Y);
      T.Remove (X, Y);
   end Remove_Record;

   ------------------
   --  Set_Record  --
   ------------------

   procedure Set_Record      (T : in out Table;
                              D : in     Table_Data_Set;
                              R : in     Natural;
                              E : in     Element_Type)
   is
      X, Y : Integer;
   begin
      T.Data_To_XY (D, R, X, Y);
      T.Put (X, Y, E);
   end Set_Record;

   -----------------
   --  Is_Sparse  --
   -----------------

   function  Is_Sparse       (T : in    Table) return Boolean is
   begin
      return T.Count < T.Length_X * T.Length_Y;
   end Is_Sparse;

   ---------------
   --  Compare  --
   ---------------

   procedure Compare         (T     : in    Table;
                              Other : in    Table;
                              Ignore_Missing_Headers : in Boolean := False;
                              Result   : out Boolean;
                              Reason   : out Comparison_Failure_Type;
                              DataSet1 : out Table_Data_Set;
                              DataSet2 : out Table_Data_Set;
                              Rec      : out Natural)
   is
      type Parsed_DS_Array is
        array (Table_Data_Set
               range Table_Data_Set'(1) .. Table_Data_Set (T.Data_Sets_Count))
        of Boolean;
      D1 : Table_Data_Set := 1;
      D2 : Table_Data_Set := 1;
      Parsed_Data_Sets : Parsed_DS_Array := (others => False);
   begin
      Result := True;
      if T.Is_Sparse or Other.Is_Sparse then
         Result := False;
         Reason := Fail_Sparse;
      elsif T.Records_Count /= Other.Records_Count then
         Result := False;
         Reason := Fail_Num_Records;
      else
         while D2 <= Table_Data_Set (Other.Data_Sets_Count) loop
            begin
               D1 := T.Data_Set_For (Other.Get_Record (D2, 0));
               Parsed_Data_Sets (D1) := True;
               for R in 1 .. T.Records_Count loop
                  if T.Get_Record (D1, R) /= Other.Get_Record (D2, R) then
                     Result := False;
                     Reason := Fail_Cell;
                     DataSet1 := D1;
                     DataSet2 := D2;
                     Rec := R;
                     return;
                  end if;
               end loop;
            exception
               when Constraint_Error =>
                  if not Ignore_Missing_Headers then
                     Result := False;
                     Reason := Fail_Missing_Header;
                     DataSet1 := 0;
                     DataSet2 := D2;
                     Rec := 0;
                     return;
                  end if;
            end;
            D2 := D2 + 1;
         end loop;
         for DS in Parsed_Data_Sets'Range loop
            if not Parsed_Data_Sets (DS) then
               Result := False;
               Reason := Fail_Missing_Header;
               DataSet1 := DS;
               DataSet2 := 0;
               Rec := 0;
               return;
            end if;
         end loop;
      end if;
   end Compare;

   -----------------
   --  Transpose  --
   -----------------

   function Transpose (T : in Table) return Table is
      Res : Table;
      I : Cursor;
      K : Key_Type;
   begin
      I := First (T);
      while Has_Element (I) loop
         K := Key (I);
         Put (Res, K.Y, K.X, Element (I));
         Next (I);
      end loop;
      case T.Header_Kind is
         when None         => Res.Set_Header_Kind (None);
         when First_Row    => Res.Set_Header_Kind (First_Column);
         when First_Column => Res.Set_Header_Kind (First_Row);
         when Transpose    => Res.Set_Header_Kind (None);
      end case;
      return Res;
   end Transpose;

   -----------------------
   --  Set_Header_Name  --
   -----------------------

   procedure Set_Header_Name (T : in out Table;
                              Old_Header, New_Header : Element_Type) is
   begin
      for I in First_Table_Data_Set .. Last_Data_Set (T) loop
         if Get_Record (T, I, 0) = Old_Header then
            Set_Record (T, I, 0, New_Header);
         end if;
      end loop;
   end Set_Header_Name;

   -----------------------
   --  Import_Data_Set  --
   -----------------------

   procedure Import_Data_Set (T : in out Table;
                              Other_Table : in Table;
                              Other_Header : Element_Type;
                              Rename : Element_Type)
   is
      D1 : constant Table_Data_Set := T.Next_Data_Set;
      D2 : constant Table_Data_Set := Other_Table.Data_Set_For (Other_Header);
      I    : Integer := 1;
      Rec  : Element_Type;
      Have : Boolean := True;
   begin
      T.Set_Record (D1, 0, Rename);
      while Have loop
         Other_Table.Get_Record (D2, I, Rec, Have);
         if Have then
            T.Set_Record (D1, I, Rec);
         end if;
         I := I + 1;
      end loop;
   end Import_Data_Set;

   -----------
   --  "="  --
   -----------

   function "=" (Left, Right : in Table) return Boolean is
      use Maps;
   begin
      return Left.First_X = Right.First_X and then
             Left.First_Y = Right.First_Y and then
             Left.Last_X  = Right.Last_X  and then
             Left.Last_Y  = Right.Last_Y  and then
             Left.Count   = Right.Count   and then
             Left.Head    = Right.Head    and then
             Left.Map     = Right.Map;
   end "=";

end XReqLib.Tables;
