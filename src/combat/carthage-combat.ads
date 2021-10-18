private with Ada.Containers.Vectors;

with Carthage.Handles.Assets;
with Carthage.Handles.Houses;
with Carthage.Handles.Planets;
with Carthage.Handles.Stacks;
with Carthage.Handles.Tiles;

package Carthage.Combat is

   procedure New_Battle
     (Attacker : Carthage.Handles.Stacks.Stack_Handle;
      Defender : Carthage.Handles.Stacks.Stack_Handle;
      Planet   : Carthage.Handles.Planets.Planet_Handle;
      Tile     : Carthage.Handles.Tiles.Tile_Handle);

   type Attack_Record is private;

   function Image (Attack : Attack_Record) return String;

   function Attacker
     (Attack : Attack_Record)
      return Carthage.Handles.Assets.Asset_Handle;

   function Defender
     (Attack : Attack_Record)
      return Carthage.Handles.Assets.Asset_Handle;

   function Weapon
     (Attack : Attack_Record)
      return Carthage.Handles.Weapon_Category;

   function Attack_Accuracy
     (Attack : Attack_Record)
      return Natural;

   function Defense_Agility
     (Attack : Attack_Record)
      return Natural;

   function Attack_Strength
     (Attack : Attack_Record)
      return Natural;

   function Defense_Strength
     (Attack : Attack_Record)
      return Natural;

   function Hit_Roll
     (Attack : Attack_Record)
      return Positive;

   function Hit
     (Attack : Attack_Record)
      return Boolean;

   function Damage_Roll
     (Attack : Attack_Record)
      return Positive;

   function Damage
     (Attack : Attack_Record)
      return Natural;

   type Battle_Record is private;

   procedure Create
     (Battle   : in out Battle_Record;
      Attacker : Carthage.Handles.Houses.House_Handle;
      Defender : Carthage.Handles.Houses.House_Handle);

   function Attacker
     (Battle : Battle_Record)
      return Carthage.Handles.Houses.House_Handle;

   function Defender
     (Battle : Battle_Record)
      return Carthage.Handles.Houses.House_Handle;

   procedure Add_Stack
     (Battle : in out Battle_Record;
      Stack  : Carthage.Handles.Stacks.Stack_Handle);

   type Attack_Record_Array is array (Positive range <>) of Attack_Record;

   function Attack_Round
     (Battle : in out Battle_Record;
      Weapon : Carthage.Handles.Weapon_Category)
      return Attack_Record_Array;

   procedure Scan_Battles
     (Process : not null access
        procedure (Battle : in out Battle_Record));

private

   type Attack_Record is
      record
         Attacker    : Carthage.Handles.Assets.Asset_Handle;
         Defender    : Carthage.Handles.Assets.Asset_Handle;
         Weapon      : Carthage.Handles.Weapon_Category;
         Accuracy    : Natural;
         Agility     : Natural;
         Strength    : Natural;
         Armour      : Natural;
         Hit         : Boolean;
         Hit_Roll    : Positive;
         Damage_Roll : Positive;
         Damage      : Natural;
      end record;

   function Attacker
     (Attack : Attack_Record)
      return Carthage.Handles.Assets.Asset_Handle
   is (Attack.Attacker);

   function Defender
     (Attack : Attack_Record)
      return Carthage.Handles.Assets.Asset_Handle
   is (Attack.Defender);

   function Weapon
     (Attack : Attack_Record)
      return Carthage.Handles.Weapon_Category
   is (Attack.Weapon);

   function Attack_Accuracy
     (Attack : Attack_Record)
      return Natural
   is (Attack.Accuracy);

   function Defense_Agility
     (Attack : Attack_Record)
      return Natural
   is (Attack.Agility);

   function Attack_Strength
     (Attack : Attack_Record)
      return Natural
   is (Attack.Strength);

   function Defense_Strength
     (Attack : Attack_Record)
      return Natural
   is (Attack.Armour);

   function Hit_Roll
     (Attack : Attack_Record)
      return Positive
   is (Attack.Hit_Roll);

   function Hit
     (Attack : Attack_Record)
      return Boolean
   is (Attack.Hit);

   function Damage_Roll
     (Attack : Attack_Record)
      return Positive
   is (Attack.Damage_Roll);

   function Damage
     (Attack : Attack_Record)
      return Natural
   is (Attack.Damage);

   package Asset_Vectors is
     new Ada.Containers.Vectors
       (Positive, Carthage.Handles.Assets.Asset_Handle,
        Carthage.Handles.Assets."=");

   package Stack_Vectors is
     new Ada.Containers.Vectors
       (Positive, Carthage.Handles.Stacks.Stack_Handle,
        Carthage.Handles.Stacks."=");

   type Battle_Record is
      record
         Active    : Boolean;
         Attacker  : Carthage.Handles.Houses.House_Handle;
         Defender  : Carthage.Handles.Houses.House_Handle;
         Planet    : Carthage.Handles.Planets.Planet_Handle;
         Tile      : Carthage.Handles.Tiles.Tile_Handle;
         Attackers : Asset_Vectors.Vector;
         Defenders : Asset_Vectors.Vector;
         Stacks    : Stack_Vectors.Vector;
      end record;

   function Attacker
     (Battle : Battle_Record)
      return Carthage.Handles.Houses.House_Handle
   is (Battle.Attacker);

   function Defender
     (Battle : Battle_Record)
      return Carthage.Handles.Houses.House_Handle
   is (Battle.Defender);

end Carthage.Combat;
