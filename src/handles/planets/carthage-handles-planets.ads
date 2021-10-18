private with Ada.Containers.Vectors;

with Ada.Streams;

with Hexes;

private with Hexes.Grids;

with Carthage.Colors;

with Carthage.Handles.Tiles;

package Carthage.Handles.Planets is

   Planet_Version : constant Object_Version := (0, 1, 0);

   subtype Coordinate is Unit_Real;

   type Planet_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface
   with private;

   function Reference
     (Handle : Planet_Handle)
      return Planet_Reference;

   function Get (Reference : Planet_Reference) return Planet_Handle;
   function Empty_Handle return Planet_Handle;

   overriding function Tag
     (Handle : Planet_Handle)
      return String;

   function Galaxy_X
     (Handle : Planet_Handle)
      return Coordinate;

   function Galaxy_Y
     (Handle : Planet_Handle)
      return Coordinate;

   function World
     (This : Planet_Handle)
      return World_Reference;

   function Category_Name
     (This : Planet_Handle)
      return String;

   function Tile_Set
     (This : Planet_Handle)
      return String;

   function Get_Tile
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Carthage.Handles.Tiles.Tile_Handle;

   function Get_Tile
     (This     : Planet_Handle;
      X        : Tile_X;
      Y        : Tile_Y)
      return Carthage.Handles.Tiles.Tile_Handle
   is (Get_Tile (This, (X, Y)));

   function To_Cubic
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Hexes.Cube_Coordinate;

   function To_Position
     (This     : Planet_Handle;
      Cubic    : Hexes.Cube_Coordinate)
      return Tile_Position;

   function Road_Cost
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Natural;

   function Terrain_Color
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Carthage.Colors.Color_Type;

   function Neighbours
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Array_Of_Positions;

   function Hex_Distance
     (This     : Planet_Handle;
      From, To : Tile_Position)
      return Natural;

   procedure Scan_Tiles
     (This    : Planet_Handle;
      Process : not null access
        procedure (Tile : Carthage.Handles.Tiles.Tile_Handle));

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
      return Array_Of_Positions;

   function Land_Connection
     (This     : Planet_Handle;
      From, To : Tile_Position)
      return Boolean;

   type Array_Of_Tiles is
     array (Positive range <>) of Carthage.Handles.Tiles.Tile_Handle;

   function Neighbour_Tiles
     (This     : Planet_Handle;
      Position : Tile_Position)
      return Array_Of_Tiles;

   procedure Scan_Connected_Tiles
     (This    : Planet_Handle;
      Start   : Tile_Position;
      Test    : not null access
        function (Tile : Carthage.Handles.Tiles.Tile_Handle) return Boolean;
      Process : not null access
        procedure (Tile : Carthage.Handles.Tiles.Tile_Handle));

   procedure Scan_Neighbours_Within
     (This     : Planet_Handle;
      Start    : Tile_Position;
      Distance : Natural;
      Process  : not null access
        procedure (Tile : Carthage.Handles.Tiles.Tile_Handle));

   --     function Find_Tile
   --       (Planet : Planet_Record;
   --        Start  : Tile_Position;
   --        Test   : not null access
   --          function (Position : Tile_Position) return Boolean)
   --        return Tile_Position;

   function Has_Owner (This : Planet_Handle) return Boolean;
   function Owner (This : Planet_Handle) return House_Reference
     with Pre => Has_Owner (This);

   procedure Set_Owner
     (This      : Planet_Handle;
      New_Owner : House_Reference);

   procedure Set_Orbital_Stack
     (This : Planet_Handle;
      House : House_Reference;
      Stack : Stack_Reference)
     with Pre => This.Orbital_Stack (House) = Null_Stack_Reference,
       Post => This.Orbital_Stack (House) = Stack;

   function Has_Palace (This : Planet_Handle) return Boolean;
   function Palace (This : Planet_Handle)
                    return City_Reference
     with Pre => Has_Palace (This);

   procedure Set_Palace
     (This   : Planet_Handle;
      Palace : City_Reference)
     with Pre => not This.Has_Palace,
     Post => This.Has_Palace
     and then This.Palace = Palace;

   function Seen_By
     (This   : Planet_Handle;
      House  : House_Reference)
      return Boolean;

   procedure Set_Seen_By
     (This   : Planet_Handle;
      House  : House_Reference);

   function Explored_By
     (This   : Planet_Handle;
      House  : House_Reference)
      return Boolean;

   procedure Set_Explored_By
     (This   : Planet_Handle;
      House  : House_Reference);

   type Surface_Tiles is private;

   procedure Get_Tiles
     (This   : Planet_Handle;
      Tiles  : out Surface_Tiles);

   procedure Get_Tiles
     (This   : Planet_Handle;
      Test   : not null access
        function (Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Boolean;
      Tiles  : out Surface_Tiles);

   procedure Get_Tiles
     (This         : Planet_Handle;
      Origin       : Carthage.Handles.Tiles.Tile_Handle;
      Min_Distance : Natural;
      Max_Distance : Natural;
      Test         : access
        function (Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Boolean;
      Tiles        : out Surface_Tiles);

   procedure Remove_Tile
     (Tiles    : in out Surface_Tiles;
      Position : Tile_Position);

   procedure Remove_Tiles
     (Tiles    : in out Surface_Tiles;
      Test     : not null access
        function (Tile : Carthage.Handles.Tiles.Tile_Handle) return Boolean);

   procedure Remove_Tiles
     (Tiles        : in out Surface_Tiles;
      Position     : Tile_Position;
      Max_Distance : Natural);

   function Tile_Count (Tiles : Surface_Tiles) return Natural;

   function Get_Tile
     (Surface : Surface_Tiles;
                      Index   : Positive)
      return Carthage.Handles.Tiles.Tile_Handle;

   function Orbital_Stack
     (This   : Planet_Handle;
      House  : House_Reference)
      return Stack_Reference;

   type Planet_Manager_Interface is interface;

   function Exists (Tag : String) return Boolean;

   function Get (Tag : String) return Planet_Handle
     with Pre => Exists (Tag);

   procedure Clear_Visibility (This : Planet_Handle);

   type Array_Of_Planets is array (Positive range <>) of Planet_Handle;

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure For_All_Cities
     (Planet : Planet_Handle;
      Process : not null access procedure (City : City_Reference));

   procedure For_All_Owned_Cities
     (Planet  : Planet_Handle;
      Owner   : House_Reference;
      Process : not null access procedure (City : City_Reference));

   procedure For_All_Owned_Stacks
     (Planet  : Planet_Handle;
      Owner   : House_Reference;
      Process : not null access procedure (Stack : Stack_Reference));

   procedure For_All_Planets
     (Process : not null access procedure (Planet : Planet_Handle));

private

   type Planet_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface with
      record
         Reference : Planet_Reference := 0;
      end record;

   overriding function Localisation_Tag
     (Handle : Planet_Handle)
      return String
   is ("planet-" & Planet_Handle'Class (Handle).Tag);

   procedure Reveal_Planet
     (This     : Planet_Handle;
      House    : House_Reference;
      Explored : Boolean);

   overriding function Short_Name
     (Handle : Planet_Handle)
      return String
   is (Handle.Tag);

   function Reference (Handle : Planet_Handle) return Planet_Reference
   is (Handle.Reference);

   function Get (Reference : Planet_Reference) return Planet_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Planet_Handle
   is (False, 0);

   package Tile_Grids is
     new Hexes.Grids (Tile_Reference, Real);

   package Tile_Vectors is
     new Ada.Containers.Vectors
       (Positive, Carthage.Handles.Tiles.Tile_Handle,
        Carthage.Handles.Tiles."=");

   type Surface_Tiles is
      record
         Planet : Planet_Handle;
         Tiles  : Tile_Vectors.Vector;
      end record;

   function Tile_Count (Tiles : Surface_Tiles) return Natural
   is (Tiles.Tiles.Last_Index);

   function Get_Tile (Surface : Surface_Tiles;
                      Index   : Positive)
                      return Carthage.Handles.Tiles.Tile_Handle
   is (Surface.Tiles.Element (Index));

   function Create_Planet
     (Tag        : String;
      X, Y       : Coordinate;
      Category   : World_Reference;
      Tile_Set   : Natural)
      return Planet_Reference;

   procedure Set_Tile_Range
     (Planet      : Planet_Handle'Class;
      First, Last : Tile_Reference);

end Carthage.Handles.Planets;
