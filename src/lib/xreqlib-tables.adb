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
         Ok := False;
         --  Elem := Elem;
   end Item;

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

   ---------------------
   --  Convert_To_XY  --
   ---------------------

   procedure Convert_To_XY   (T    : in     Table;
                              Row  : in     Positive;
                              Col  : in     Positive;
                              X    : out    Integer;
                              Y    : out    Integer) is
   begin
      if T.Head = Transpose then
         Y := Col + T.First_Y - 1;
         X := Row + T.First_X - 1;
      else
         Y := Row + T.First_Y - 1;
         X := Col + T.First_X - 1;
      end if;
   end Convert_To_XY;

   ----------------------
   --  Convert_To_Pos  --
   ----------------------

   procedure Convert_To_Pos  (T    : in     Table;
                              X    : in     Integer;
                              Y    : in     Integer;
                              Row  : out    Positive;
                              Col  : out    Positive) is
   begin
      if T.Head = Transpose then
         Col := Y - T.First_Y + 1;
         Row := X - T.First_X + 1;
      else
         Row := Y - T.First_Y + 1;
         Col := X - T.First_X + 1;
      end if;
   end Convert_To_Pos;

   -------------
   --  Width  --
   -------------

   function  Width           (T    : in     Table) return Natural is
   begin
      if T.Head = Transpose then
         return T.Length_Y;
      else
         return T.Length_X;
      end if;
   end Width;

   --------------
   --  Height  --
   --------------

   function  Height          (T    : in     Table) return Natural is
   begin
      if T.Head = Transpose then
         return T.Length_X;
      else
         return T.Length_Y;
      end if;
   end Height;

   -----------
   --  Get  --
   -----------

   function  Get             (T    : in     Table;
                              Row  : in     Positive;
                              Col  : in     Positive) return Element_Type is
      X, Y : Integer;
   begin
      T.Convert_To_XY (Row, Col, X, Y);
      return T.Item (X, Y);
   end Get;
   -----------
   --  Set  --
   -----------

   procedure Set             (T    : in out Table;
                              Row  : in     Positive;
                              Col  : in     Positive;
                              Elem : in     Element_Type) is
      X, Y : Integer;
   begin
      T.Convert_To_XY (Row, Col, X, Y);
      T.Put (X, Y, Elem);
   end Set;

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

   ------------------
   --  Get_Record  --
   ------------------

   function  Get_Record      (T : in    Table;
                              D : in    Table_Data_Set;
                              R : in    Natural) return Element_Type
   is
      C : constant Integer := Integer (D) - 1;
   begin
      if T.Head = Transpose or T.Head = First_Column then
         return T.Item (T.First_X + R, T.First_Y + C);
      else
         return T.Item (T.First_X + C, T.First_Y + R);
      end if;
   end Get_Record;

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
