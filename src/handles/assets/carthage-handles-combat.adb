with WL.Random;

with Hira.Db;

with Reiko.Updates;

with Carthage.Calendar;
with Carthage.Logging;

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

   type Battle_Update is
     new Reiko.Root_Update_Type with
      record
         Battle_Index : Positive;
         Weapon       : Carthage.Handles.Units.Weapon_Category;
      end record;

   overriding function Name
     (Update : Battle_Update)
      return String
   is ("battle");

   overriding procedure Execute
     (Update : Battle_Update);

   procedure Log_Attacker
     (Battle : Battle_Record;
      Message : String)
   is null;

   procedure Log_Defender
     (Battle  : Battle_Record;
      Message : String)
   is null;

   ---------------
   -- Add_Stack --
   ---------------

   procedure Add_Stack
     (Battle : in out Battle_Record;
      Stack  : Carthage.Handles.Stacks.Stack_Handle)
   is
      Attacking : constant Boolean := Stack.House.Tag = Battle.Attacker.Tag;
      Defending : constant Boolean := Stack.House.Tag = Battle.Defender.Tag;
   begin
      if not Attacking and then not Defending then
         Carthage.Handles.Stacks.Log (Stack, "neutral during battle");
         return;
      elsif Attacking then
         Carthage.Handles.Stacks.Log (Stack, "attacking");
      else
         Carthage.Handles.Stacks.Log (Stack, "defending");
      end if;

      for Asset of Hira.Asset.Select_By_Stack (Stack) loop
         if Attacking then
            Carthage.Handles.Assets.Log (Asset, "attacker asset");
            Battle.Attackers.Append (Asset.To_Asset_Handle);
         else
            Carthage.Handles.Assets.Log (Asset, "defender asset");
            Battle.Defenders.Append (Asset.To_Asset_Handle);
         end if;
      end loop;

      Battle.Stacks.Append (Stack.To_Stack_Handle);

   end Add_Stack;

   ------------------
   -- Attack_Round --
   ------------------

   function Attack_Round
     (Battle : in out Battle_Record;
      Weapon : Carthage.Handles.Units.Weapon_Category)
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
         if Carthage.Handles.Units.Has_Attack (Asset.Unit, Weapon) then
            declare
               use all type Carthage.Handles.Units.Weapon_Category;
               Defender : constant Carthage.Handles.Assets.Asset_Handle :=
                            Choose_Target (Targets);
               Attack : Attack_Record;
            begin
               if not Defender.Has_Element then
                  Carthage.Handles.Assets.Log (Asset, "no targets");
                  return;
               end if;

               Attack := Attack_Record'
                 (Attacker        => Asset.To_Asset_Handle,
                  Defender        => Defender.To_Asset_Handle,
                  Weapon          => Weapon,
                  Accuracy        =>
                    Carthage.Handles.Units.Accuracy (Asset.Unit, Weapon),
                  Agility         => Defender.Unit.Agility,
                  Strength        =>
                    Carthage.Handles.Units.Strength (Asset.Unit, Weapon),
                  Armour          =>
                    (if Weapon = Psy
                     then Defender.Unit.Psy_Defence
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
         Target : Carthage.Handles.Assets.Asset_Handle;
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
         if Carthage.Handles.Assets.Alive (Asset)
           and then Carthage.Handles.Units.Can_Target
             (Weapon, Asset.Unit.Category)
         then
            Attacker_Targets.Append (Asset);
         end if;
      end loop;

      for Asset of Battle.Attackers loop
         if Carthage.Handles.Assets.Alive (Asset)
           and then Carthage.Handles.Units.Can_Target
             (Weapon, Asset.Unit.Category)
         then
            Defender_Targets.Append (Asset);
         end if;
      end loop;

      for Asset of Battle.Attackers loop
         if Carthage.Handles.Assets.Alive (Asset) then
            Check (Asset, Attacker_Targets);
         end if;
      end loop;
      for Asset of Battle.Defenders loop
         if Carthage.Handles.Assets.Alive (Asset) then
            Check (Asset, Defender_Targets);
         end if;
      end loop;

      for I in 1 .. Count loop
         if Carthage.Handles.Assets.Alive (Result (I).Defender)
           and then Result (I).Damage > 0
         then
            Carthage.Handles.Assets.Damage
              (Result (I).Defender, Result (I).Damage);
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
      Battle.Attacker := Attacker.To_House_Handle;
      Battle.Defender := Defender.To_House_Handle;
      Battle.Attackers.Clear;
      Battle.Defenders.Clear;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Battle_Update)
   is
      use Carthage.Handles.Units;
      use all type Hira.Db.Weapon_Category;
      Battle : Battle_Record renames Current_Battles (Update.Battle_Index);
      Weapon : Weapon_Category := Update.Weapon;
   begin

      if not Battle.Active then
         return;
      end if;

      Log_Attacker (Battle, "attacking " & Defender (Battle).Tag);

      loop

         Log_Attacker (Battle, "phase: " & Weapon'Image);

         declare
            Round : constant Attack_Record_Array :=
                      Carthage.Combat.Attack_Round (Battle, Weapon);
         begin

            for Attack of Round loop
               Log_Attacker (Battle, Image (Attack));
            end loop;

            exit when Round'Length > 0;

            if Weapon = Weapon_Category'Last then
               Weapon := Weapon_Category'First;
            else
               Weapon := Weapon_Category'Succ (Weapon);
            end if;

            if Weapon = Update.Weapon then
               Battle.Active := False;
               exit;
            end if;
         end;
      end loop;

      for Stack of Battle.Stacks loop
         for Asset of Hira.Asset.Select_By_Stack (Stack) loop
            if not Carthage.Handles.Assets.Alive (Asset) then
               Carthage.Handles.Assets.Log (Asset, "destroyed");
            end if;
         end loop;
      end loop;

      for Stack of Battle.Stacks loop
         Carthage.Handles.Stacks.Remove_Dead_Assets (Stack);
      end loop;

      if Battle.Active then
         declare
            New_Update : Battle_Update := Update;
         begin
            if Weapon = Weapon_Category'Last then
               Weapon := Weapon_Category'First;
            else
               Weapon := Weapon_Category'Succ (Weapon);
            end if;

            New_Update.Weapon := Weapon;
            Reiko.Updates.Add_Update
              (New_Update, Reiko.Reiko_Duration (Carthage.Calendar.Days (1)));
         end;
      end if;

   end Execute;

   -----------
   -- Image --
   -----------

   function Image (Attack : Attack_Record) return String is
   begin
      return Carthage.Handles.Assets.Description (Attack.Attacker)
        & " vs "
        & Carthage.Handles.Assets.Description (Attack.Defender)
        & " using " & Attack.Weapon'Image
        & ": acc" & Attack.Accuracy'Image
        & " ag" & Attack.Agility'Image
        & " str" & Attack.Strength'Image
        & " arm" & Attack.Armour'Image
        & " roll" & Attack.Hit_Roll'Image
        & (if Attack.Hit
           then ": hit: dmg roll" & Attack.Damage_Roll'Image
           & " dmg" & Attack.Damage'Image
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
      Create (Battle, Attacker.House, Defender.House);
      Battle.Planet := Planet.To_Planet_Handle;
      Battle.Tile := Tile;
      Battle.Active := True;
      Add_Stack (Battle, Attacker);
      Add_Stack (Battle, Defender);
      Current_Battles.Append (Battle);
      Reiko.Updates.Add_Update
        (Update       => Battle_Update'
           (Reiko.Root_Update_Type with
            Battle_Index => Current_Battles.Last_Index,
            Weapon       => Carthage.Handles.Units.Weapon_Category'First),
         Update_Delay =>
           Reiko.Reiko_Duration (Carthage.Calendar.Days (1)));
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
               use Carthage.Handles.Assets;
               Attackers : Natural := 0;
               Defenders : Natural := 0;
            begin
               for Asset of Battle.Attackers loop
                  if Alive (Asset) then
                     Attackers := Attackers + 1;
                  end if;
               end loop;
               for Asset of Battle.Defenders loop
                  if Alive (Asset) then
                     Defenders := Defenders + 1;
                  end if;
               end loop;

               if Defenders = 0 then
                  if Attackers = 0 then
                     Log_Attacker
                       (Battle,
                        Battle.Attacker.Tag & " vs " & Battle.Defender.Tag
                        & ": mutual destruction");
                  else
                     Log_Attacker
                       (Battle,
                        "Victory against "
                        & Carthage.Handles.Houses.Full_Name (Battle.Defender));
                  end if;
                  Battle.Active := False;
               elsif Attackers = 0 then
                  Log_Defender
                    (Battle,
                     "Victory against "
                     & Carthage.Handles.Houses.Full_Name (Battle.Attacker));
                  Battle.Active := False;
               end if;
            end;
         end if;
      end loop;

      for Battle of Current_Battles loop
         if Battle.Active then
            declare
               use Carthage.Handles.Tiles;
               Name      : constant String :=
                             "The Battle of "
                             & Carthage.Handles.Planets.Name (Battle.Planet)
                             & (if Carthage.Handles.Tiles.Has_Element
                                (Battle.Tile)
                                then " at " & Description (Battle.Tile)
                                else "");
            begin
               Carthage.Logging.Log (Name);
            end;
            Process (Battle);
         end if;
      end loop;
   end Scan_Battles;

end Carthage.Combat;
