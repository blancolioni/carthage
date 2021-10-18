with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

with WL.String_Maps;

package body Carthage.Handles.Units is

   package Unit_Vectors is
     new Ada.Containers.Vectors (Real_Unit_Reference, Unit_Record);

   package Unit_Maps is
     new WL.String_Maps (Unit_Reference);

   Unit_Vector : Unit_Vectors.Vector;
   Unit_Map    : Unit_Maps.Map;

   function Get
     (Handle : Unit_Handle)
      return Unit_Vectors.Constant_Reference_Type
   is (Unit_Vector (Handle.Reference));

   overriding function Tag
     (Handle : Unit_Handle)
      return String
   is (-Get (Handle).Tag);

   function Resource_Name
     (This : Unit_Handle)
      return String
   is ("unit" & Integer'Image (-Integer (This.Reference)));

   function Category
     (This : Unit_Handle)
      return Unit_Category
   is (Get (This).Category);

   function Movement
     (This : Unit_Handle)
      return Natural
   is (Get (This).Move);

   function Spot
     (This : Unit_Handle)
      return Natural
   is (Get (This).Spot);

   function Eat
     (This : Unit_Handle)
      return Carthage.Quantities.Quantity_Type
   is (Get (This).Eat);

   function Agility
     (This : Unit_Handle)
      return Natural
   is (Get (This).Agility);

   function Armour
     (This : Unit_Handle)
      return Natural
   is (Get (This).Armour);

   function Psychic_Defense
     (This : Unit_Handle)
      return Natural
   is (Get (This).Psy_Defence);

   function Rank
     (This : Unit_Handle)
      return Natural
   is (Get (This).Rank);

   function Cargo_Capacity
     (This : Unit_Handle)
      return Natural
   is (Get (This).Cargo);

   function Maintenance
     (This : Unit_Handle)
      return Carthage.Money.Money_Type
   is (Get (This).Maintenance);

   function Revenue
     (This : Unit_Handle)
      return Carthage.Money.Money_Type
   is (Get (This).Revenue);

   function Has_Attack
     (This   : Unit_Handle;
      Weapon : Weapon_Category)
      return Boolean
   is (Get (This).Weapons (Weapon).Strength > 0);

   function Accuracy
     (This   : Unit_Handle;
      Weapon : Weapon_Category)
      return Positive
   is (Get (This).Weapons (Weapon).Accuracy);

   function Strength
     (This   : Unit_Handle;
      Weapon : Weapon_Category)
      return Positive
   is (Get (This).Weapons (Weapon).Strength);

   function Is_Cargo_Pod
     (This : Unit_Handle)
      return Boolean
   is (This.Tag = "cargo");

   function Exists (Tag : String) return Boolean
   is (Unit_Map.Contains (Tag));

   function Get (Tag : String) return Unit_Handle
   is (Get (Unit_Map.Element (Tag)));

   ---------------
   -- Cargo_Pod --
   ---------------

   function Cargo_Pod return Unit_Handle is
   begin
      if not Cargo_Pod_Handle.Has_Element then
         Cargo_Pod_Handle := Get ("cargo");
         pragma Assert (Cargo_Pod_Handle.Has_Element);
      end if;
      return Cargo_Pod_Handle;
   end Cargo_Pod;

   ------------
   -- Create --
   ------------

   procedure Create (Rec : Unit_Record) is
   begin
      Unit_Vector.Append (Rec);
      Unit_Map.Insert (-Rec.Tag, Unit_Vector.Last_Index);
      if Ada.Strings.Fixed.Index (-Rec.Tag, "cargo") > 0 then
         Cargo_Pod_Handle := Get (Unit_Vector.Last_Index);
      end if;
      Get (Unit_Vector.Last_Index).Log
        ("created with index" & Rec.Index'Image);
   end Create;

   ------------------
   -- Get_By_Index --
   ------------------

   function Get_By_Index (Index : Positive) return Unit_Handle is
   begin
      for Reference in 1 .. Unit_Vector.Last_Index loop
         if Unit_Vector (Reference).Index = Index then
            return Get (Reference);
         end if;
      end loop;
      raise Constraint_Error with
        "no such unit index:" & Index'Image;
   end Get_By_Index;

   --------------------
   -- Is_Valid_Index --
   --------------------

   function Is_Valid_Index (Index : Positive) return Boolean is
   begin
      return Index <= Natural (Unit_Vector.Last_Index);
   end Is_Valid_Index;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Unit_Vectors.Vector'Read (Stream, Unit_Vector);
      for Reference in 1 .. Unit_Vector.Last_Index loop
         Unit_Map.Insert (Get (Reference).Tag, Reference);
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Unit_Vectors.Vector'Write (Stream, Unit_Vector);
   end Save;

end Carthage.Handles.Units;
