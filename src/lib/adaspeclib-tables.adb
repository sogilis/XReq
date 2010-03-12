package body AdaSpecLib.Tables is

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Key_Type) return Boolean is
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
      Insert (T.Map, Key_Type'(X, Y), Elem);
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

end AdaSpecLib.Tables;