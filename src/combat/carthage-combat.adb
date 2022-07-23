with WL.Random;

with Carthage.Logging;

with Carthage.Handles.Units;

package body Carthage.Combat is

   Under_Damage : constant array (1 .. 5, 1 .. 10) of Positive :=
                    (1 => (10, 15, 20, 25, 30, 40, 50, 60, 70, 80),
                     2 => (5, 10, 15, 20, 25, 30, 40, 50, 60, 70),
                     3 => (1, 5, 10, 15, 20, 25, 30, 40, 50, 60),
                     4 => (1, 3, 5, 10, 15, 20, 25, 30, 40, 50),
                     5 => (1, 2, 3, 5, 10, 15, 20, 25, 30, 40));

   Over_Damage  : constant array (1 .. 8, 1 .. 10) of Positive :=
                    (1 => (10, 15, 20, 25, 30, 40, 50, 60, 70, 80),
                     2 => (15, 20, 25, 30, 40, 50, 60, 70, 80, 90),
                     3 => (20, 25, 30, 40, 50, 60, 70, 80, 90, 100),
                     4 => (25, 30, 40, 50, 60, 70, 80, 90, 100, 100),
                     5 => (30, 40, 50, 60, 70, 80, 90, 100, 100, 100),
                     6 => (40, 50, 60, 70, 80, 90, 100, 100, 100, 100),
                     7 => (50, 60, 70, 80, 90, 100, 100, 100, 100, 100),
                     8 => (60, 70, 80, 90, 100, 100, 100, 100, 100, 100));

   package Battle_Vectors is
     new Ada.Containers.Vectors (Positive, Battle_Record);

   Current_Battles : Battle_Vectors.Vector;

   procedure Resolve
     (Attack : in out Attack_Record);

   ---------------
   -- Add_Stack --
   ---------------

   procedure Add_Stack
     (Battle : in out Battle_Record;
      Stack  : Carthage.Handles.Stacks.Stack_Handle)
   is
      use type Carthage.Handles.Houses.House_Handle;
      Attacking : constant Boolean := Stack.Owner = Battle.Attacker;
      Defending : constant Boolean := Stack.Owner = Battle.Defender;
   begin
      if not Attacking and then not Defending then
         Stack.Log ("neutral during battle");
         return;
      elsif Attacking then
         Stack.Log ("attacking");
      else
         Stack.Log ("defending");
      end if;

      for I in 1 .. Stack.Asset_Count loop
         declare
            Asset : constant Carthage.Handles.Assets.Asset_Handle :=
                      Carthage.Handles.Assets.Get (Stack.Asset (I));
         begin
            if Attacking then
               Asset.Log ("attacker asset");
               Battle.Attackers.Append (Asset);
            else
               Asset.Log ("defender asset");
               Battle.Defenders.Append (Asset);
            end if;
         end;
      end loop;

      Battle.Stacks.Append (Stack);

   end Add_Stack;

   ------------------
   -- Attack_Round --
   ------------------

   function Attack_Round
     (Battle : in out Battle_Record;
      Weapon : Carthage.Handles.Weapon_Category)
      return Attack_Record_Array
   is
      Max    : constant Natural :=
                 Battle.Attackers.Last_Index
                   + Battle.Defenders.Last_Index;
      Result : Attack_Record_Array (1 .. Max);
      Count  : Natural := 0;

      procedure Check
        (Asset   : Carthage.Handles.Assets.Asset_Handle;
         Targets : Asset_Vectors.Vector);

      function Choose_Target
        (Targets : Asset_Vectors.Vector)
         return Carthage.Handles.Assets.Asset_Handle;

      -----------
      -- Check --
      -----------

      procedure Check
        (Asset   : Carthage.Handles.Assets.Asset_Handle;
         Targets : Asset_Vectors.Vector)
      is
      begin
         if Asset.Unit.Has_Attack (Weapon) then
            declare
               use all type Carthage.Handles.Weapon_Category;
               Defender : constant Carthage.Handles.Assets.Asset_Handle :=
                            Choose_Target (Targets);
               Attack : Attack_Record;
            begin
               if not Defender.Has_Element then
                  Asset.Log ("no targets");
                  return;
               end if;

               Attack := Attack_Record'
                 (Attacker        => Asset,
                  Defender        => Defender,
                  Weapon          => Weapon,
                  Accuracy        => Asset.Unit.Accuracy (Weapon),
                  Agility         => Defender.Unit.Agility,
                  Strength        => Asset.Unit.Strength (Weapon),
                  Armour          =>
                    (if Weapon = Psy
                     then Defender.Unit.Psychic_Defense
                     else Defender.Unit.Armour),
                  Hit_Roll        =>
                    WL.Random.Random_Number (1, 20),
                  Hit             => False,
                  Damage_Roll     =>
                    WL.Random.Random_Number (1, 10),
                  Damage          => 0);

               Resolve (Attack);

               Count := Count + 1;
               Result (Count) := Attack;
            end;
         end if;
      end Check;

      -------------------
      -- Choose_Target --
      -------------------

      function Choose_Target
        (Targets : Asset_Vectors.Vector)
         return Carthage.Handles.Assets.Asset_Handle
      is
         Lowest : Natural := Natural'Last;
         Target : Carthage.Handles.Assets.Asset_Handle :=
                    Carthage.Handles.Assets.Empty_Handle;
      begin
         for Possible of Targets loop
            if Possible.Health > 0
              and then Possible.Unit.Rank < Lowest
            then
               Target := Possible;
               Lowest := Possible.Unit.Rank;
            end if;
         end loop;

         return Target;
      end Choose_Target;

      Attacker_Targets : Asset_Vectors.Vector;
      Defender_Targets : Asset_Vectors.Vector;

   begin

      for Asset of Battle.Defenders loop
         if Asset.Alive
           and then Carthage.Handles.Units.Can_Target
             (Weapon, Asset.Unit.Category)
         then
            Attacker_Targets.Append (Asset);
         end if;
      end loop;

      for Asset of Battle.Attackers loop
         if Asset.Alive
           and then Carthage.Handles.Units.Can_Target
             (Weapon, Asset.Unit.Category)
         then
            Defender_Targets.Append (Asset);
         end if;
      end loop;

      for Asset of Battle.Attackers loop
         if Asset.Alive then
            Check (Asset, Attacker_Targets);
         end if;
      end loop;
      for Asset of Battle.Defenders loop
         if Asset.Alive then
            Check (Asset, Defender_Targets);
         end if;
      end loop;

      for I in 1 .. Count loop
         if Result (I).Defender.Alive
           and then Result (I).Damage > 0
         then
            Result (I).Defender.Damage
              (Result (I).Damage);
         end if;
      end loop;

      return Result (1 .. Count);

   end Attack_Round;

   ------------
   -- Create --
   ------------

   procedure Create
     (Battle   : in out Battle_Record;
      Attacker : Carthage.Handles.Houses.House_Handle;
      Defender : Carthage.Handles.Houses.House_Handle)
   is
   begin
      Battle.Active := True;
      Battle.Attacker := Attacker;
      Battle.Defender := Defender;
      Battle.Attackers.Clear;
      Battle.Defenders.Clear;
   end Create;

   -----------
   -- Image --
   -----------

   function Image (Attack : Attack_Record) return String is
   begin
      return Attack.Attacker.Long_Name & " vs "
        & Attack.Defender.Long_Name
        & " using " & Attack.Weapon'Img
        & ": acc" & Attack.Accuracy'Img
        & " ag" & Attack.Agility'Img
        & " str" & Attack.Strength'Img
        & " arm" & Attack.Armour'Img
        & " roll" & Attack.Hit_Roll'Img
        & (if Attack.Hit
           then ": hit: dmg roll" & Attack.Damage_Roll'Img
           & " dmg" & Attack.Damage'Img
           else ": miss");
   end Image;

   ----------------
   -- New_Battle --
   ----------------

   procedure New_Battle
     (Attacker : Carthage.Handles.Stacks.Stack_Handle;
      Defender : Carthage.Handles.Stacks.Stack_Handle;
      Planet   : Carthage.Handles.Planets.Planet_Handle;
      Tile     : Carthage.Handles.Tiles.Tile_Handle)
   is
      Battle : Battle_Record;
   begin
      Create (Battle, Attacker.Owner, Defender.Owner);
      Battle.Planet := Planet;
      Battle.Tile := Tile;
      Battle.Active := True;
      Add_Stack (Battle, Attacker);
      Add_Stack (Battle, Defender);
      Current_Battles.Append (Battle);
   end New_Battle;

   -------------
   -- Resolve --
   -------------

   procedure Resolve
     (Attack : in out Attack_Record)
   is
   begin
      Attack.Hit :=
        Attack.Hit_Roll + Attack.Accuracy - Attack.Agility
          >= 10;
      if Attack.Hit then
         declare
            Over : constant Positive :=
                     Natural'Min
                       (Natural'Max (Attack.Strength / Attack.Armour, 1),
                        8);
            Under : constant Positive :=
                      Natural'Min
                        (Natural'Max (Attack.Armour / Attack.Strength, 1),
                         5);
         begin
            if Over > 1 then
               Attack.Damage := Over_Damage (Over, Attack.Damage_Roll);
            else
               Attack.Damage := Under_Damage (Under, Attack.Damage_Roll);
            end if;
         end;
      end if;
   end Resolve;

   ------------------
   -- Scan_Battles --
   ------------------

   procedure Scan_Battles
     (Process : not null access
        procedure (Battle : in out Battle_Record))
   is
   begin
      for Battle of Current_Battles loop
         if Battle.Active then
            declare
               Attackers : Natural := 0;
               Defenders : Natural := 0;
               Name      : constant String :=
                             "The Battle of " & Battle.Planet.Local_Text
                             & (if not Battle.Tile.Has_Element then ""
                                else " at " & Battle.Tile.Description);
            begin
               for Asset of Battle.Attackers loop
                  if Asset.Alive then
                     Attackers := Attackers + 1;
                  end if;
               end loop;
               for Asset of Battle.Defenders loop
                  if Asset.Alive then
                     Defenders := Defenders + 1;
                  end if;
               end loop;

               if Defenders = 0 then
                  if Attackers = 0 then
                     Battle.Attacker.Log
                       (Name & " vs " & Battle.Defender.Local_Text
                        & ": mutual destruction");
                  else
                     Battle.Attacker.Log
                       (Name & ": victory against "
                        & Battle.Defender.Local_Text);
                  end if;
                  Battle.Active := False;
               elsif Attackers = 0 then
                  Battle.Defender.Log
                    (Name & ": victory against "
                     & Battle.Attacker.Local_Text);
                  Battle.Active := False;
               end if;
            end;
         end if;
      end loop;

      for Battle of Current_Battles loop
         if Battle.Active then
            declare
               Name      : constant String :=
                             "The Battle of " & Battle.Planet.Local_Text
                             & (if not Battle.Tile.Has_Element then ""
                                else " at " & Battle.Tile.Description);
            begin
               Carthage.Logging.Log (Name);
            end;
            Process (Battle);
         end if;
      end loop;
   end Scan_Battles;

   -------------------
   -- Update_Battle --
   -------------------

   procedure Update_Battle (Battle : in out Battle_Record) is
      use Carthage.Handles;
      Weapon : constant Weapon_Category := Battle.Next_Weapon;
   begin

      if not Battle.Active then
         return;
      end if;

      Attacker (Battle).Log ("attacking " & Defender (Battle).Short_Name);

      loop

         Attacker (Battle).Log ("phase: " & Weapon'Img);

         declare
            Round : constant Attack_Record_Array :=
                      Carthage.Combat.Attack_Round (Battle, Weapon);
         begin

            for Attack of Round loop
               Attacker (Battle).Log (Image (Attack));
            end loop;

            exit when Round'Length > 0;

         end;
      end loop;

      for Stack of Battle.Stacks loop
         for I in 1 .. Stack.Asset_Count loop
            declare
               Asset : constant Carthage.Handles.Assets.Asset_Handle :=
                         Carthage.Handles.Assets.Get (Stack.Asset (I));
            begin
               if not Asset.Alive then
                  Asset.Log ("destroyed");
               end if;
            end;
         end loop;
      end loop;

      for Stack of Battle.Stacks loop
         Stack.Remove_Dead_Assets;
      end loop;

      if Battle.Active then
         if Battle.Next_Weapon = Weapon_Category'Last then
            Battle.Next_Weapon := Weapon_Category'First;
         else
            Battle.Next_Weapon := Weapon_Category'Succ (Battle.Next_Weapon);
         end if;
      end if;

   end Update_Battle;

end Carthage.Combat;
