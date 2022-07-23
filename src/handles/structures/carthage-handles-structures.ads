private with Ada.Containers.Vectors;
with Ada.Streams;

with Carthage.Handles.Resources;
with Carthage.Handles.Stocks;

with Carthage.Quantities;

package Carthage.Handles.Structures is

   Chance_Against : constant := 1024;

   Structure_Version : constant Object_Version := (0, 1, 0);

   type Structure_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface
   with private;

   function Reference (Handle : Structure_Handle) return Structure_Reference;
   function Get (Reference : Structure_Reference) return Structure_Handle;
   function Empty_Handle return Structure_Handle;

   function Has_Production
     (This    : Structure_Handle)
      return Boolean;

   type Resource_Quantity_Record is
      record
         Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type;
      end record;

   type Production_Array is
     array (Positive range <>) of Resource_Quantity_Record;

   function Production_Inputs
     (This    : Structure_Handle)
      return Production_Array;

   function Production_Output
     (This    : Structure_Handle)
      return Carthage.Handles.Resources.Resource_Handle;

   function Production_Quantity
     (This    : Structure_Handle)
      return Carthage.Quantities.Quantity_Type;

   function Produces
     (This      : Structure_Handle;
      Resource  : Carthage.Handles.Resources.Resource_Handle)
      return Boolean;

   function Consumes
     (This      : Structure_Handle;
      Resource  : Carthage.Handles.Resources.Resource_Handle)
      return Boolean;

   function Is_Agora
     (This      : Structure_Handle)
      return Boolean;

   function Is_Bonus
     (This      : Structure_Handle)
      return Boolean;

   function Is_Harvester
     (This      : Structure_Handle)
      return Boolean;

   function Is_Palace
     (This      : Structure_Handle)
      return Boolean;

   function Is_Shield
     (This      : Structure_Handle)
      return Boolean;

   function Radius
     (This      : Structure_Handle)
      return Natural;

   function Harvest_Production
     (This      : Structure_Handle;
      Tile      : Tile_Reference)
      return Production_Array
     with Pre => This.Is_Harvester;

   function Execute_Production
     (This       : Structure_Handle;
      House      : House_Reference;
      Tile       : Tile_Reference;
      Stock      : Carthage.Handles.Stocks.Stock_Handle_Interface'Class;
      Efficiency : Unit_Real;
      Factor     : Unit_Real)
      return Carthage.Quantities.Quantity_Type;

   function Exists (Tag : String) return Boolean;

   function Get (Tag : String) return Structure_Handle
     with Pre => Exists (Tag);

   function Get_By_Index (Index : Positive) return Structure_Handle;

   function Palace return Structure_Handle
     with Post => Palace'Result /= Empty_Handle;

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

private

   type Structure_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface with
      record
         Reference : Structure_Reference := 0;
      end record;

   overriding function Tag
     (Handle : Structure_Handle)
      return String;

   overriding function Localisation_Tag
     (Handle : Structure_Handle)
      return String
   is ("structure-" & Structure_Handle'Class (Handle).Tag);

   overriding function Short_Name
     (Handle : Structure_Handle)
      return String
   is (Handle.Tag);

   procedure Add_Bonus_Production
     (Structure : Structure_Handle;
      Bonus     : Structure_Reference;
      Resource  : Resource_Reference;
      Quantity  : Natural);

   function Reference (Handle : Structure_Handle) return Structure_Reference
   is (Handle.Reference);

   function Get (Reference : Structure_Reference) return Structure_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Structure_Handle
   is (False, 0);

   Palace_Handle : Structure_Handle := Empty_Handle;

   function Palace return Structure_Handle
   is (Palace_Handle);

   type Harvest_Production_Record is
      record
         World    : World_Reference;
         City     : Boolean;
         Terrain  : Terrain_Reference;
         Resource : Resource_Reference;
         Quantity : Carthage.Quantities.Quantity_Type;
      end record;

   package Harvest_Production_Vectors is
     new Ada.Containers.Vectors (Positive, Harvest_Production_Record);

   type Bonus_Production_Record is
      record
         Bonus    : Structure_Reference;
         Resource : Resource_Reference;
         Quantity : Natural;
      end record;

   package Bonus_Production_Vectors is
     new Ada.Containers.Vectors (Positive, Bonus_Production_Record);

   type Structure_Record is
      record
         Tag                 : Ada.Strings.Unbounded.Unbounded_String;
         Index               : Positive;
         Singular            : Boolean;
         Can_Build           : Boolean;
         Palace              : Boolean;
         Shield              : Boolean;
         Church              : Boolean;
         Agora               : Boolean;
         Water               : Boolean;
         Land                : Boolean;
         Road                : Boolean;
         Barren              : Boolean;
         Neutral             : Boolean;
         Is_Harvester        : Boolean;
         Is_Bonus            : Boolean;
         Area                : Natural;
         Maintenance         : Natural;
         Cost                : Natural;
         Build_Time          : Natural;
         Enabled_By          : Technology_Reference;
         Value               : Natural;
         Production_Inputs   : Carthage.Handles.Resources.Resource_Stock;
         Production_Output   : Resource_Reference;
         Production_Quantity : Carthage.Quantities.Quantity_Type;
         Bonus_Production    : Bonus_Production_Vectors.Vector;
         Harvest_Production  : Harvest_Production_Vectors.Vector;
      end record;

   procedure Create (Structure : Structure_Record);

end Carthage.Handles.Structures;
