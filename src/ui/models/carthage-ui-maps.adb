with Ada.Containers.Doubly_Linked_Lists;

--  with WL.Images.FLC;
with WL.String_Maps;

with Carthage.Handles.Cities;
with Carthage.Handles.Structures;
with Carthage.Handles.Terrain;
with Carthage.Handles.Tiles;
with Carthage.Handles.Worlds;

package body Carthage.UI.Maps is

   type Direction is
     (North, Northeast, Southeast, South, Southwest, Northwest);

   function Direction_Of
     (From, To : Tile_Position) return Direction;

   type Direction_Flags is mod 2 ** 6;

   Powers : constant array (Direction) of Direction_Flags :=
              (1, 2, 4, 8, 16, 32);

   package Direction_Maps is
     new WL.String_Maps (Direction);

   Direction_Map : Direction_Maps.Map;

   function To_Direction
     (Name : String)
      return Direction
   is (Direction_Map.Element (Name));

   function To_Flag
     (Name : String)
      return Direction_Flags
   is (Powers (To_Direction (Name)));

   function To_Flags
     (Planet : Carthage.Handles.Planets.Planet_Handle;
      Start  : Tile_Position;
      Test   : not null access
        function (Position : Tile_Position) return Boolean)
      return Direction_Flags;

   type Tile_Index is new Natural;

   type Tile_Index_Array is array (Direction_Flags) of Tile_Index;

   type Tile_Resource_Record is
      record
         Join     : Boolean := False;
         Base     : Tile_Index       := 0;
         Default  : Tile_Index       := 0;
         Always   : Tile_Index       := 0;
         Indices  : Tile_Index_Array := (others => 0);
      end record;

   type Tile_Resource_Access is access Tile_Resource_Record;

   package Tile_Resource_Maps is
     new WL.String_Maps (Tile_Resource_Access);

   Tile_Map : Tile_Resource_Maps.Map;

   package Terrain_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Carthage.Handles.Terrain.Terrain_Handle,
        Carthage.Handles.Terrain."=");

   Feature_Priority : Terrain_Lists.List;

   procedure Get_Terrain_Resources
     (Planet        : Carthage.Handles.Planets.Planet_Handle;
      Tile          : Carthage.Handles.Tiles.Tile_Handle;
      Layers        : in out Tile_Layers'Class);

   function Make_Resource_Name
     (Planet : Carthage.Handles.Planets.Planet_Handle;
      Tile   : Tile_Index)
      return String
   is (Carthage.Handles.Planets.Tile_Set (Planet)
       & Integer'Image (-(Integer (Tile))) & "-tile");

   function Make_Background_Resource
     (Resource_Name : String;
      Color        : Carthage.Colors.Color_Type)
      return Layer_Element
   is (Resource_Name'Length, Background_Hex_Tile,
       Resource_Name, Color);

   function Make_Hex_Tile_Resource
     (Resource_Name : String)
      return Layer_Element
   is (Resource_Name'Length, Hex_Tile, Resource_Name, (0.0, 0.0, 0.0, 0.0));

--     procedure Check_Unit_Resource
--       (Renderer      : in out Lui.Rendering.Root_Renderer'Class;
--        Resource_Name : String)
--       with Unreferenced;

   -------------------------
   -- Check_Unit_Resource --
   -------------------------

--     procedure Check_Unit_Resource
--       (Renderer      : in out Lui.Rendering.Root_Renderer'Class;
--        Resource_Name : String)
--     is
--     begin
--        if not Renderer.Have_Resource (Resource_Name) then
--           declare
--              Reader : WL.Images.FLC.FLC_Image_Reader;
--              Image  : WL.Images.Image_Type;
--           begin
--              Reader.Read
--                (Carthage.Configure.Fading_Suns_FLC_File
--                   (Resource_Name),
--                 Image);
--              Renderer.Create_Image_Resource (Resource_Name, Image);
--           end;
--        end if;
--     end Check_Unit_Resource;

   ------------------------------
   -- Configure_Tile_Resources --
   ------------------------------

   procedure Configure_Tile_Resources
     (Config : Tropos.Configuration)
   is
      procedure Load_Terrain_Tiles
        (Terrain : Carthage.Handles.Terrain.Terrain_Handle);

      ------------------------
      -- Load_Terrain_Tiles --
      ------------------------

      procedure Load_Terrain_Tiles
        (Terrain : Carthage.Handles.Terrain.Terrain_Handle)
      is
         Name : constant String := Terrain.Tag;
      begin
         if Config.Contains (Terrain.Tag) then
            declare
               Tile_Config : constant Tropos.Configuration :=
                               Config.Child (Name);
               Item : constant Tile_Resource_Access :=
                               new Tile_Resource_Record;
            begin
               for Item_Config of Tile_Config loop
                  declare
                     Key : constant String := Item_Config.Config_Name;
                  begin
                     if Key = "default" then
                        Item.Default :=
                          Tile_Index (Natural'(Item_Config.Value));
                     elsif Key = "base" then
                        Item.Base :=
                          Tile_Index (Natural'(Item_Config.Value));
                     elsif Key = "all" then
                        Item.Indices (Direction_Flags'Last) :=
                          Tile_Index (Natural'(Item_Config.Value));
                     elsif Key = "none" then
                        Item.Indices (Direction_Flags'First) :=
                          Tile_Index (Natural'(Item_Config.Value));
                     elsif Key = "fade" then
                        null;
                     else
                        declare
                           Index : constant Tile_Index :=
                                     Tile_Index'Value (Key);
                           Flag  : Direction_Flags := 0;
                        begin
                           for D_Config of Item_Config loop
                              Flag := Flag +
                                To_Flag (D_Config.Config_Name);
                           end loop;
                           Item.Indices (Flag) := Index;
                        end;
                     end if;
                  end;
               end loop;

               Tile_Map.Insert (Name, Item);

            end;
         end if;
      end Load_Terrain_Tiles;

   begin
      for D in Direction loop
         Direction_Map.Insert (Direction'Image (D), D);
      end loop;

      Direction_Map.Insert ("n", North);
      Direction_Map.Insert ("ne", Northeast);
      Direction_Map.Insert ("nw", Northwest);
      Direction_Map.Insert ("s", South);
      Direction_Map.Insert ("se", Southeast);
      Direction_Map.Insert ("sw", Southwest);

      for Feature_Config of Config.Child ("feature-priority") loop
         Feature_Priority.Append
           (Carthage.Handles.Terrain.Get (Feature_Config.Config_Name));
      end loop;

      Carthage.Handles.Terrain.For_All_Terrain (Load_Terrain_Tiles'Access);
   end Configure_Tile_Resources;

   ------------------
   -- Direction_Of --
   ------------------

   function Direction_Of
     (From, To : Tile_Position) return Direction
   is
   begin
      if From.X = To.X then
         if From.Y < To.Y then
            return South;
         else
            return North;
         end if;
      elsif From.X < To.X then
         if From.Y < To.Y
           or else (From.Y = To.Y and then From.X mod 2 = 0)
         then
            return Southeast;
         else
            return Northeast;
         end if;
      else
         if From.Y < To.Y
           or else (From.Y = To.Y and then From.X mod 2 = 0)
         then
            return Southwest;
         else
            return Northwest;
         end if;
      end if;
   end Direction_Of;

   ---------------------------
   -- Get_Terrain_Resources --
   ---------------------------

   procedure Get_Terrain_Resources
     (Planet        : Carthage.Handles.Planets.Planet_Handle;
      Tile          : Carthage.Handles.Tiles.Tile_Handle;
      Layers        : in out Tile_Layers'Class)
   is

      use Carthage.Handles.Terrain;

      Base_Layer : Carthage.Handles.Tiles.Terrain_Layer := 1;
      Feature_Layer : Carthage.Handles.Tiles.Terrain_Layer := 2;

      Base : Terrain_Handle := Tile.Terrain (Base_Layer);
      Feature : Terrain_Handle := Tile.Terrain (Feature_Layer);

      Position : constant Tile_Position := Tile.Position;

      Feature_Index : Tile_Index;
      Base_Index    : Tile_Index;

      procedure Add
        (Index      : Tile_Index);

      ---------
      -- Add --
      ---------

      procedure Add
        (Index : Tile_Index)
      is
      begin
         if Index /= 0 then
            Layers.List.Append
              (Make_Hex_Tile_Resource
                 (Make_Resource_Name (Planet, Index)));
         end if;
      end Add;

   begin

      for Priority of Feature_Priority loop
         if Priority.Tag = Base.Tag then
            Base := Feature;
            Feature := Priority;
            Base_Layer := 2;
            Feature_Layer := 1;
            exit;
         end if;
         exit when Priority = Feature;
      end loop;

      if not Base.Has_Element
        or else not Tile_Map.Contains (Base.Tag)
      then
         Base_Index := 0;
      else
         declare
            Tile_Info : constant Tile_Resource_Access :=
                          Tile_Map.Element (Base.Tag);

         begin
            Base_Index := Tile_Info.Base;
            Add (Base_Index);

            if Base_Index /= 0 then
               declare
                  Ns     : constant Array_Of_Positions :=
                             Carthage.Handles.Planets.Neighbours
                               (Planet, Tile.Position);
               begin
                  for Neighbour_Pos of Ns loop
                     declare
                        use Carthage.Handles.Tiles;
                        Neighbour : constant Tile_Handle :=
                                      Planet.Get_Tile (Neighbour_Pos);
                     begin
                        if Neighbour.Terrain (Feature_Layer) /= Feature then
                           Add (Base_Index + 1 +
                                  Direction'Pos
                                    (Direction_Of
                                       (Position, Neighbour_Pos)));
                        end if;
                     end;
                  end loop;
               end;
            end if;

         end;
      end if;

      if not Feature.Has_Element
        or else not Tile_Map.Contains (Feature.Tag)
      then
         Feature_Index := 0;
      else
         declare
            Tile_Info : constant Tile_Resource_Access :=
                          Tile_Map.Element (Feature.Tag);

            function Same_Terrain
              (Neighbour : Tile_Position)
               return Boolean
            is (Carthage.Handles.Planets.Get_Tile (Planet, Neighbour)
                  .Terrain (Feature_Layer)
                = Feature);

            Flags : constant Direction_Flags :=
                      To_Flags
                        (Planet => Planet,
                         Start  => Tile.Position,
                         Test   => Same_Terrain'Access);
         begin
            if Tile_Info.Indices (Flags) = 0 then
               Feature_Index := Tile_Info.Default;
            else
               Feature_Index := Tile_Info.Indices (Flags);
            end if;
         end;
      end if;

      Add (Feature_Index);

   end Get_Terrain_Resources;

   ---------------------
   -- Get_Tile_Layers --
   ---------------------

   procedure Get_Tile_Layers
     (Planet   : Carthage.Handles.Planets.Planet_Handle;
      House    : Carthage.Handles.Houses.House_Handle;
      Position : Tile_Position;
      Layers   : in out Tile_Layers'Class)
   is
      use Carthage.Handles.Tiles;

      Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
               Carthage.Handles.Planets.Get_Tile (Planet, Position);

      procedure Add
        (Index      : Tile_Index);

      ---------
      -- Add --
      ---------

      procedure Add
        (Index : Tile_Index)
      is
      begin
         if Index /= 0 then
            Layers.List.Append
              (Make_Hex_Tile_Resource
                 (Make_Resource_Name (Planet, Index)));
         end if;
      end Add;

   begin

      if not Wizard_Mode
        and then not Carthage.Handles.Tiles.Seen_By (Tile, House)
      then
         Layers.List.Append
           (Make_Background_Resource
              ("unknown-hex-tile", (0.0, 0.0, 0.0, 1.0)));
         return;
      end if;

      Get_Terrain_Resources (Planet, Tile, Layers);

      if Carthage.Handles.Tiles.Has_Road (Tile) then

         --  Add (67);

         for Neighbour_Tile of
           Carthage.Handles.Planets.Neighbour_Tiles (Planet, Position)
         loop
            if Carthage.Handles.Tiles.Has_Road (Neighbour_Tile) then
               case Direction_Of (Position, Neighbour_Tile.Position) is
                  when Northwest =>
                     Add (68);
                  when South =>
                     Add (69);
                  when Southeast =>
                     Add (70);
                  when North =>
                     Add (71);
                  when Southwest =>
                     Add (72);
                  when Northeast =>
                     Add (73);
               end case;
            end if;
         end loop;

      end if;

      if Tile.Has_City then
         declare
            City : constant Carthage.Handles.Cities.City_Handle :=
                     Carthage.Handles.Cities.Get (Tile.Get_City);
         begin
            if Wizard_Mode
              or else Carthage.Handles.Tiles.Explored_By (Tile, House)
              or else Carthage.Handles.Structures.Get (City.Structure).Is_Bonus
            then
               Layers.List.Append
                 (Make_Hex_Tile_Resource
                    (Carthage.Handles.Worlds.Get (Planet.World).Tag
                     & "-"
                     & Carthage.Handles.Structures.Get (City.Structure).Tag
                     & "-"
                     & "hex"));
            end if;
         end;
      end if;

      if Carthage.Handles.Tiles.Explored_By (Tile, House) then
         Layers.List.Append
           (Make_Background_Resource
              ("explored-shadow-hex-tile", (0.0, 0.0, 0.0, 0.2)));
      else
         Layers.List.Append
           (Make_Background_Resource
              ("shadow-hex-tile", (0.0, 0.0, 0.0, 0.4)));
      end if;

   end Get_Tile_Layers;

   -----------------
   -- Scan_Layers --
   -----------------

   procedure Scan_Layers
     (Layers  : Tile_Layers'Class;
      Process : not null access
        procedure (Element : Layer_Element_Type;
                   Resource_Name : String;
                   Color : Carthage.Colors.Color_Type))
   is
   begin
      for Element of Layers.List loop
         Process (Element.Layer_Element,
                  Element.Resource_Name, Element.Color);
      end loop;
   end Scan_Layers;

   --------------
   -- To_Flags --
   --------------

   function To_Flags
     (Planet : Carthage.Handles.Planets.Planet_Handle;
      Start  : Tile_Position;
      Test   : not null access
        function (Position : Tile_Position) return Boolean)
      return Direction_Flags
   is
      Ns : constant Array_Of_Positions :=
             Carthage.Handles.Planets.Neighbours (Planet, Start);
      Result : Direction_Flags := 0;
   begin
      for N of Ns loop
         if Test (N) then
            Result := Result + Powers (Direction_Of (Start, N));
         end if;
      end loop;
      return Result;
   end To_Flags;

end Carthage.UI.Maps;
