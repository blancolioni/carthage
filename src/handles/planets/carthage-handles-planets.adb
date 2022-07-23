with WL.String_Maps;

with Carthage.Handles.Cities;
with Carthage.Handles.Houses;
with Carthage.Handles.Stacks;
with Carthage.Handles.Terrain;
with Carthage.Handles.Worlds;

with Carthage.Handles.Vectors;

package body Carthage.Handles.Planets is

   type Orbital_Stack_Array is
     array (Real_House_Reference) of Stack_Reference;

   type Planet_Record is
      record
         Tag            : Ada.Strings.Unbounded.Unbounded_String;
         X, Y           : Coordinate;
         Category       : World_Reference;
         Tile_Set       : Natural;
         Owner          : House_Reference := Null_House_Reference;
         Palace         : City_Reference := Null_City_Reference;
         Grid           : Tile_Grids.Hex_Grid;
         Seen           : Set_Of_Houses := Empty_House_Set;
         Explored       : Set_Of_Houses := Empty_House_Set;
         Orbital_Stacks : Orbital_Stack_Array :=
                            (others => Null_Stack_Reference);
      end record;

   package Planet_Vectors is
     new Carthage.Handles.Vectors
       (Real_Planet_Reference, Planet_Record, "planet");

   package Planet_Maps is
     new WL.String_Maps (Planet_Reference);

   Planet_Vector : Planet_Vectors.Vector;
   Planet_Map    : Planet_Maps.Map;

   function Get
     (Handle : Planet_Handle)
      return Planet_Vectors.Constant_Reference_Type
   is (Planet_Vector (Handle.Reference));

   function Exists (Tag : String) return Boolean
   is (Planet_Map.Contains (Tag));

   function Get (Tag : String) return Planet_Handle
   is (Get (Planet_Map (Tag)));

   overriding function Tag
     (Handle : Planet_Handle)
      return String
   is (-Get (Handle).Tag);

   function Galaxy_X
     (Handle : Planet_Handle)
      return Coordinate
   is (Get (Handle).X);

   function Galaxy_Y
     (Handle : Planet_Handle)
      return Coordinate
   is (Get (Handle).Y);

   function World
     (This : Planet_Handle)
      return World_Reference
   is (Get (This).Category);

   function Category_Name
     (This : Planet_Handle)
      return String
   is (Carthage.Handles.Worlds.Get (Get (This).Category).Tag);

   function Tile_Set
     (This : Planet_Handle)
      return String
   is (if This.Category_Name = "desert"
       then "barren"
       else This.Category_Name);

   function Has_Owner (This : Planet_Handle) return Boolean
   is (Get (This).Owner /= Null_House_Reference);

   function Owner (This : Planet_Handle) return House_Reference
   is (Get (This).Owner);

   function Has_Palace (This : Planet_Handle) return Boolean
   is (Get (This).Palace /= Null_City_Reference);

   function Palace (This : Planet_Handle)
                    return City_Reference
   is (Get (This).Palace);

   function Explored_By
     (This   : Planet_Handle;
      House  : House_Reference)
      return Boolean
   is (Carthage.Handles.Houses.Get (House).Is_Element_Of
         (Get (This).Explored));

   function Seen_By
     (This   : Planet_Handle;
      House  : House_Reference)
      return Boolean
   is (Carthage.Handles.Houses.Get (House).Is_Element_Of
       (Get (This).Seen));

   function Orbital_Stack
     (This   : Planet_Handle;
      House  : House_Reference)
      return Stack_Reference
   is (Get (This).Orbital_Stacks (House));

   ----------------------
   -- Clear_Visibility --
   ----------------------

   procedure Clear_Visibility (This : Planet_Handle) is

      procedure Update (Rec : in out Planet_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Planet_Record) is

         procedure Clear_Tile
           (Tile : Tile_Reference);

         ----------------
         -- Clear_Tile --
         ----------------

         procedure Clear_Tile
           (Tile : Tile_Reference) is
         begin
            Carthage.Handles.Tiles.Get (Tile).Clear_Visibility;
         end Clear_Tile;

      begin
         Clear (Rec.Seen);
         Clear (Rec.Explored);
         Rec.Grid.Scan_Tiles (Clear_Tile'Access);
      end Update;

   begin

      Planet_Vector.Update (This.Reference, Update'Access);

   end Clear_Visibility;

   function Create_Planet
     (Tag        : String;
      X, Y       : Coordinate;
      Category   : World_Reference;
      Tile_Set   : Natural)
      return Planet_Reference
   is
      Rec : Planet_Record := Planet_Record'
        (Tag            => +Tag,
         X              => X,
         Y              => Y,
         Category       => Category,
         Tile_Set       => Tile_Set,
         Owner          => Null_House_Reference,
         Palace         => Null_City_Reference,
         Grid           => <>,
         Seen           => <>,
         Explored       => <>,
         Orbital_Stacks => <>);

      Grid : Tile_Grids.Hex_Grid renames Rec.Grid;

   begin
      Grid.Create_Square_Grid
        (Width             => Planet_Width,
         Height            => Planet_Height,
         Horizontal_Wrap   => True,
         Vertical_Wrap     => False,
         Has_Vertical_Axis => True,
         Offset_Odd        => False);

      return Reference : Planet_Reference do
         Planet_Vector.Append (Rec, Reference);
         Planet_Map.Insert (Tag, Reference);
      end return;

   end Create_Planet;

   ---------------
   -- Find_Path --
   ---------------

   function Find_Path
     (This     : Planet_Handle;
      Start    : Tile_Position;
      Finish   : Tile_Position;
      Passable : not null access
        function (Tile : Tile_Reference)
      return Boolean;
      Cost     : not null access
        function (Tile : Tile_Reference)
      return Non_Negative_Real)
      return Array_Of_Positions
   is
      Path : constant Hexes.Cube_Coordinate_Array :=
               Get (This).Grid.Find_Path
               (Start    => To_Cubic (This, Start),
                Finish   => To_Cubic (This, Finish),
                Passable => Passable,
                Cost     => Cost);
   begin
      return Result : Array_Of_Positions (Path'Range) do
         for I in Result'Range loop
            Result (I) := This.To_Position (Path (I));
         end loop;
      end return;
   end Find_Path;

   --------------------
   -- For_All_Cities --
   --------------------

   procedure For_All_Cities
     (Planet  : Planet_Handle;
      Process : not null access procedure (City : City_Reference))
   is
      procedure Process_Tile (Reference : Tile_Reference);

      ------------------
      -- Process_Tile --
      ------------------

      procedure Process_Tile (Reference : Tile_Reference) is
         Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                  Carthage.Handles.Tiles.Get (Reference);
      begin
         if Tile.Has_City then
            Process (Tile.Get_City);
         end if;
      end Process_Tile;

   begin
      Get (Planet).Grid.Scan_Tiles (Process_Tile'Access);
   end For_All_Cities;

   --------------------------
   -- For_All_Owned_Cities --
   --------------------------

   procedure For_All_Owned_Cities
     (Planet  : Planet_Handle;
      Owner   : House_Reference;
      Process : not null access procedure (City : City_Reference))
   is
      procedure Process_Tile (Reference : Tile_Reference);

      ------------------
      -- Process_Tile --
      ------------------

      procedure Process_Tile (Reference : Tile_Reference) is
         Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                  Carthage.Handles.Tiles.Get (Reference);
      begin
         if Tile.Has_City
           and then Carthage.Handles.Cities.Get (Tile.Get_City).Owner.Reference
           = Owner
         then
            Process (Tile.Get_City);
         end if;
      end Process_Tile;

   begin
      Get (Planet).Grid.Scan_Tiles (Process_Tile'Access);
   end For_All_Owned_Cities;

   procedure For_All_Owned_Stacks
     (Planet  : Planet_Handle;
      Owner   : House_Reference;
      Process : not null access procedure (Stack : Stack_Reference))
   is
      procedure Process_Tile (Reference : Tile_Reference);

      ------------------
      -- Process_Tile --
      ------------------

      procedure Process_Tile (Reference : Tile_Reference) is
         Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                  Carthage.Handles.Tiles.Get (Reference);

         procedure Process_Stack (Stack : Stack_Reference);

         -------------------
         -- Process_Stack --
         -------------------

         procedure Process_Stack (Stack : Stack_Reference) is
         begin
            if Carthage.Handles.Stacks.Get (Stack).Owner.Reference = Owner then
               Process (Stack);
            end if;
         end Process_Stack;

      begin
         if Tile.Has_Stacks then
            Tile.Scan_Stacks (Process_Stack'Access);
         end if;
      end Process_Tile;

   begin
      Get (Planet).Grid.Scan_Tiles (Process_Tile'Access);
   end For_All_Owned_Stacks;

   ---------------------
   -- For_All_Planets --
   ---------------------

   procedure For_All_Planets
     (Process : not null access procedure (Planet : Planet_Handle))
   is
   begin
      for Reference in 1 .. Planet_Vector.Last_Index loop
         Process (Get (Reference));
      end loop;
   end For_All_Planets;

   --------------
   -- Get_Tile --
   --------------

   function Get_Tile
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Carthage.Handles.Tiles.Tile_Handle
   is
   begin
      return Carthage.Handles.Tiles.Get
        (Get (This).Grid.Get_Tile
         (To_Cubic (This, Position)));
   end Get_Tile;

   ---------------
   -- Get_Tiles --
   ---------------

   procedure Get_Tiles
     (This   : Planet_Handle;
      Tiles  : out Surface_Tiles)
   is
      function OK
        (Tile : Carthage.Handles.Tiles.Tile_Handle)
         return Boolean;

      --------
      -- OK --
      --------

      function OK
        (Tile : Carthage.Handles.Tiles.Tile_Handle)
         return Boolean
      is
         pragma Unreferenced (Tile);
      begin
         return True;
      end OK;

   begin
      Get_Tiles (This, OK'Access, Tiles);
   end Get_Tiles;

   ---------------
   -- Get_Tiles --
   ---------------

   procedure Get_Tiles
     (This   : Planet_Handle;
      Test   : not null access
        function (Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Boolean;
      Tiles  : out Surface_Tiles)
   is
      procedure Check (Tile : Tile_Reference);

      -----------
      -- Check --
      -----------

      procedure Check (Tile : Tile_Reference) is
      begin
         if Test (Carthage.Handles.Tiles.Get (Tile)) then
            Tiles.Tiles.Append (Carthage.Handles.Tiles.Get (Tile));
         end if;
      end Check;

   begin
      Tiles.Planet := This;
      Get (This).Grid.Scan_Tiles (Check'Access);
   end Get_Tiles;

   ---------------
   -- Get_Tiles --
   ---------------

   procedure Get_Tiles
     (This         : Planet_Handle;
      Origin       : Carthage.Handles.Tiles.Tile_Handle;
      Min_Distance : Natural;
      Max_Distance : Natural;
      Test         : access
        function (Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Boolean;
      Tiles        : out Surface_Tiles)
   is
      Distance : array (Tile_X, Tile_Y) of Integer :=
                   (others => (others => -1));
      Queue    : Tile_Vectors.Vector;
      Start    : Positive := 1;
      Last     : Positive;
      Org      : constant Tile_Position := Origin.Position;
   begin
      Tiles.Planet := This;
      Queue.Append (Origin);
      Distance (Org.X, Org.Y) := 0;

      for I in 1 .. Max_Distance loop
         Last := Queue.Last_Index;

         for J in Start .. Last loop
            declare
               T : constant Carthage.Handles.Tiles.Tile_Handle := Queue (J);
               P : constant Tile_Position := T.Position;
               Current_D : constant Integer := Distance (P.X, P.Y);
            begin
               if Current_D in Min_Distance .. Max_Distance then
                  Tiles.Tiles.Append (T);
               end if;

               for Neighbour of Neighbour_Tiles (This, P) loop
                  if Test = null
                    or else Test (Neighbour)
                  then
                     declare
                        P : constant Tile_Position := Neighbour.Position;
                        D : Integer renames
                              Distance (P.X, P.Y);
                     begin
                        if D = -1
                          or else Current_D + 1 < D
                        then
                           D := Current_D + 1;
                           if D < Max_Distance then
                              Queue.Append (Neighbour);
                           end if;
                        end if;
                     end;
                  end if;
               end loop;
            end;
         end loop;
         Start := Last + 1;
      end loop;
   end Get_Tiles;

   ------------------
   -- Hex_Distance --
   ------------------

   function Hex_Distance
     (This     : Planet_Handle;
      From, To : Tile_Position)
      return Natural
   is
      pragma Unreferenced (This);
      DX : constant Natural :=
             Natural'Min (abs (From.X - To.X),
                          abs (Planet_Width - Tile_X'Max (From.X, To.X)
                            + Tile_X'Min (From.X, To.X)));
   begin
      return DX + abs (Integer (From.Y - To.Y));
--        return Tile_Graphs.Vertex_Count
--          (Surface_Graph.Shortest_Path (Index_Of (From), Index_Of (To)));
   end Hex_Distance;

   ------------------
   -- Hex_Distance --
   ------------------

   function Hex_Distance
     (This     : Planet_Handle;
      From, To : Tile_Reference)
      return Natural
   is
   begin
      return This.Hex_Distance
        (Carthage.Handles.Tiles.Get (From).Position,
         Carthage.Handles.Tiles.Get (To).Position);
   end Hex_Distance;

   ------------------
   -- Hex_Distance --
   ------------------

   function Hex_Distance
     (This     : Planet_Handle;
      From, To : Carthage.Handles.Tiles.Tile_Handle)
      return Natural
   is
   begin
      return This.Hex_Distance (From.Position, To.Position);
   end Hex_Distance;

   ---------------------
   -- Land_Connection --
   ---------------------

   function Land_Connection
     (This     : Planet_Handle;
      From, To : Tile_Position)
      return Boolean
   is
      function Is_Land
        (Tile : Tile_Reference)
         return Boolean
      is (not Carthage.Handles.Tiles.Get (Tile).Is_Water);

      function Constant_Cost
        (Tile : Tile_Reference)
         return Real;

      -------------------
      -- Constant_Cost --
      -------------------

      function Constant_Cost
        (Tile : Tile_Reference)
         return Real
      is
         pragma Unreferenced (Tile);
      begin
         return 1.0;
      end Constant_Cost;

      Path : constant Hexes.Cube_Coordinate_Array :=
               Get (This).Grid.Find_Path
               (Start    => To_Cubic (This, From),
                Finish   => To_Cubic (This, To),
                Passable => Is_Land'Access,
                Cost     => Constant_Cost'Access);
   begin
      return From = To or else Path'Length > 1;
   end Land_Connection;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Planet_Vector.Read (Stream);
      for Reference in 1 .. Planet_Vector.Last_Index loop
         Planet_Map.Insert (Get (Reference).Tag, Reference);
      end loop;
   end Load;

   ---------------------
   -- Neighbour_Tiles --
   ---------------------

   function Neighbour_Tiles
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Array_Of_Tiles
   is
      Positions : constant Array_Of_Positions :=
                    Neighbours (This, Position);
      Tiles     : Array_Of_Tiles (Positions'Range);
   begin
      for I in Positions'Range loop
         Tiles (I) := Get_Tile (This, Positions (I));
      end loop;
      return Tiles;
   end Neighbour_Tiles;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Array_Of_Positions
   is
      Cubics : constant Hexes.Cube_Coordinate_Array :=
                 Get (This).Grid.Neighbours
                 (To_Cubic (This, Position));
   begin
      return Result : Array_Of_Positions (Cubics'Range) do
         for I in Result'Range loop
            Result (I) := To_Position (This, Cubics (I));
         end loop;
      end return;
   end Neighbours;

   -----------------
   -- Remove_Tile --
   -----------------

   procedure Remove_Tile
     (Tiles    : in out Surface_Tiles;
      Position : Tile_Position)
   is
      use type Ada.Containers.Count_Type;
      Found : Boolean := False;
   begin
      if Tiles.Tiles.Is_Empty then
         return;
      end if;

      declare
         Last  : constant Carthage.Handles.Tiles.Tile_Handle :=
                   Tiles.Tiles.Last_Element;
      begin
         if Last.Position = Position then
            Found := True;
         else
            for Tile of Tiles.Tiles loop
               if Tile.Position = Position then
                  Found := True;
                  Tile := Last;
               end if;
            end loop;
         end if;

         if Found then
            Tiles.Tiles.Set_Length (Tiles.Tiles.Length - 1);
         end if;
      end;
   end Remove_Tile;

   ------------------
   -- Remove_Tiles --
   ------------------

   procedure Remove_Tiles
     (Tiles    : in out Surface_Tiles;
      Test     : not null access
        function (Tile : Carthage.Handles.Tiles.Tile_Handle) return Boolean)
   is
      New_Tiles : Tile_Vectors.Vector;
   begin
      for Tile of Tiles.Tiles loop
         if not Test (Tile) then
            New_Tiles.Append (Tile);
         end if;
      end loop;
      Tiles.Tiles := New_Tiles;
   end Remove_Tiles;

   ------------------
   -- Remove_Tiles --
   ------------------

   procedure Remove_Tiles
     (Tiles        : in out Surface_Tiles;
      Position     : Tile_Position;
      Max_Distance : Natural)
   is
      Removed : array (1 .. Planet_Width, 1 .. Planet_Height) of Boolean :=
                  (others => (others => False));
      Rs      : Tile_Vectors.Vector;
      Start   : Positive := 1;
      Count   : Natural := 0;
      Planet  : Planet_Handle renames Tiles.Planet;
   begin
      Rs.Append (Get_Tile (Planet, Position));
      Removed (Position.X, Position.Y) := True;
      for D in 1 .. Max_Distance loop
         Count := Rs.Last_Index;
         for I in Start .. Count loop
            for N of Planet.Neighbours (Rs.Element (I).Position) loop
               if not Removed (N.X, N.Y) then
                  Removed (N.X, N.Y) := True;
                  Rs.Append (Planet.Get_Tile (N));
               end if;
            end loop;
         end loop;
         Start := Count + 1;
      end loop;

      for R of Rs loop
         Remove_Tile (Tiles, R.Position);
      end loop;
   end Remove_Tiles;

   -------------------
   -- Reveal_Planet --
   -------------------

   procedure Reveal_Planet
     (This     : Planet_Handle;
      House    : House_Reference;
      Explored : Boolean)
   is
      procedure Reveal_Tile (Tile : Tile_Reference);

      -----------------
      -- Reveal_Tile --
      -----------------

      procedure Reveal_Tile (Tile : Tile_Reference) is
         T : constant Carthage.Handles.Tiles.Tile_Handle :=
               Carthage.Handles.Tiles.Get (Tile);
         H : constant Carthage.Handles.Houses.House_Handle :=
               Carthage.Handles.Houses.Get (House);
      begin
         if Explored then
            T.Set_Explored_By (H);
         end if;

         T.Set_Seen_By (H);
      end Reveal_Tile;

   begin
      Get (This).Grid.Scan_Tiles (Reveal_Tile'Access);
   end Reveal_Planet;

   ---------------
   -- Road_Cost --
   ---------------

   function Road_Cost
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Natural
   is
   begin
      return Cost : Natural := 0 do
         for Terrain of This.Get_Tile (Position).Terrain_Layers loop
            Cost := Cost + Terrain.Road_Cost (This.Category_Name);
         end loop;
      end return;
   end Road_Cost;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Planet_Vector.Write (Stream);
   end Save;

   --------------------------
   -- Scan_Connected_Tiles --
   --------------------------

   procedure Scan_Connected_Tiles
     (This    : Planet_Handle;
      Start   : Tile_Position;
      Test    : not null access
        function (Tile : Carthage.Handles.Tiles.Tile_Handle) return Boolean;
      Process : not null access
        procedure (Tile : Carthage.Handles.Tiles.Tile_Handle))
   is
      function Test_Reference (Reference : Tile_Reference) return Boolean
      is (Test (Carthage.Handles.Tiles.Get (Reference)));

      procedure Process_Reference (Reference : Tile_Reference);

      -----------------------
      -- Process_Reference --
      -----------------------

      procedure Process_Reference (Reference : Tile_Reference) is
      begin
         Process (Carthage.Handles.Tiles.Get (Reference));
      end Process_Reference;

   begin
      Get (This).Grid
        .Scan_Connected_Tiles
          (To_Cubic (This, Start),
           Test_Reference'Access, Process_Reference'Access);
   end Scan_Connected_Tiles;

   ----------------------------
   -- Scan_Neighbours_Within --
   ----------------------------

   procedure Scan_Neighbours_Within
     (This     : Planet_Handle;
      Start    : Tile_Position;
      Distance : Natural;
      Process  : not null access
        procedure (Tile : Carthage.Handles.Tiles.Tile_Handle))
   is
      Ns : Surface_Tiles;
   begin
      Get_Tiles (This, Get_Tile (This, Start), 1, Distance, null, Ns);
      for I in 1 .. Tile_Count (Ns) loop
         Process (Get_Tile (Ns, I));
      end loop;
   end Scan_Neighbours_Within;

   ----------------
   -- Scan_Tiles --
   ----------------

   procedure Scan_Tiles
     (This    : Planet_Handle;
      Process : not null access
        procedure (Tile : Carthage.Handles.Tiles.Tile_Handle))
   is
      procedure Process_Reference (Reference : Tile_Reference);

      -----------------------
      -- Process_Reference --
      -----------------------

      procedure Process_Reference (Reference : Tile_Reference) is
      begin
         Process (Carthage.Handles.Tiles.Get (Reference));
      end Process_Reference;

   begin
      Get (This).Grid.Scan_Tiles (Process_Reference'Access);
   end Scan_Tiles;

   ---------------------
   -- Set_Explored_By --
   ---------------------

   procedure Set_Explored_By
     (This   : Planet_Handle;
      House  : House_Reference)
   is
      procedure Update (Rec : in out Planet_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Planet_Record) is
      begin
         Carthage.Handles.Houses.Get (House).Include (Rec.Explored);
         Carthage.Handles.Houses.Get (House).Include (Rec.Seen);
      end Update;

   begin
      if not This.Explored_By (House) then
         Planet_Vector.Update (This.Reference, Update'Access);
         This.Reveal_Planet (House, Explored => True);
      end if;
   end Set_Explored_By;

   -----------------------
   -- Set_Orbital_Stack --
   -----------------------

   procedure Set_Orbital_Stack
     (This  : Planet_Handle;
      House : House_Reference;
      Stack : Stack_Reference)
   is
      procedure Update (Rec : in out Planet_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Planet_Record) is
      begin
         Rec.Orbital_Stacks (House) := Stack;
      end Update;

   begin
      Planet_Vector.Update (This.Reference, Update'Access);
   end Set_Orbital_Stack;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (This      : Planet_Handle;
      New_Owner : House_Reference)
   is
      procedure Update (Rec : in out Planet_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Planet_Record) is
      begin
         Rec.Owner := New_Owner;
      end Update;

   begin
      Planet_Vector.Update (This.Reference, Update'Access);
   end Set_Owner;

   ----------------
   -- Set_Palace --
   ----------------

   procedure Set_Palace
     (This   : Planet_Handle;
      Palace : City_Reference)
   is
      procedure Update (Rec : in out Planet_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Planet_Record) is
      begin
         Rec.Palace := Palace;
      end Update;

   begin
      Planet_Vector.Update (This.Reference, Update'Access);
   end Set_Palace;

   -----------------
   -- Set_Seen_By --
   -----------------

   procedure Set_Seen_By
     (This   : Planet_Handle;
      House  : House_Reference)
   is
      procedure Update (Rec : in out Planet_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Planet_Record) is
      begin
         Carthage.Handles.Houses.Get (House).Include (Rec.Seen);
      end Update;

   begin
      if not This.Seen_By (House) then
         Planet_Vector.Update (This.Reference, Update'Access);
         This.Reveal_Planet (House, Explored => False);
      end if;
   end Set_Seen_By;

   --------------------
   -- Set_Tile_Range --
   --------------------

   procedure Set_Tile_Range
     (Planet      : Planet_Handle'Class;
      First, Last : Tile_Reference)
   is
      procedure Update (Rec : in out Planet_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Planet_Record) is
      begin
         for Reference in First .. Last loop
            declare
               Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                        Carthage.Handles.Tiles.Get (Reference);
               X    : constant Tile_X := Tile.Position.X;
               Y    : constant Tile_Y := Tile.Position.Y;
            begin
               Rec.Grid.Set_Tile
                 (Rec.Grid.To_Cube_Coordinate
                    (Hexes.Distance_Type (X - 1),
                     Hexes.Distance_Type (Y - 1)),
                  Reference);
            end;
         end loop;

      end Update;

   begin
      Planet_Vector.Update (Planet.Reference, Update'Access);
   end Set_Tile_Range;

   -------------------
   -- Terrain_Color --
   -------------------

   function Terrain_Color
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Carthage.Colors.Color_Type
   is
      Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
               This.Get_Tile (Position);
   begin
      return Tile.Base_Terrain.Color (This.Tile_Set);
   end Terrain_Color;

   --------------
   -- Tile_Set --
   --------------

   --  function Tile_Set
   --    (This : Planet_Handle)
   --     return String
   --  is
   --  begin
   --     if This.Category.Tag = "city" then
   --
   --      then "city"
   --      elsif This.Category.Tag = "desert"
   --      then "barren"
   --      else This.Category.Tag);

   --------------
   -- To_Cubic --
   --------------

   function To_Cubic
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Hexes.Cube_Coordinate
   is
   begin
      return Get (This).Grid.To_Cube_Coordinate
        (Hexes.Distance_Type (Position.X - 1),
         Hexes.Distance_Type (Position.Y - 1));
   end To_Cubic;

   -----------------
   -- To_Position --
   -----------------

   function To_Position
     (This     : Planet_Handle;
      Cubic    : Hexes.Cube_Coordinate)
      return Tile_Position
   is
      Offset : constant Hexes.Offset_Coordinate :=
                 Get (This).Grid.To_Offset_Coordinate (Cubic);
   begin
      return Tile_Position'
        (X => Tile_X_Count (Hexes.Offset_X (Offset)) + 1,
         Y => Tile_Y_Count (Hexes.Offset_Y (Offset)) + 1);
   end To_Position;

end Carthage.Handles.Planets;
