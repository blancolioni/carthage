with Ada.Exceptions;
with Ada.Strings.Fixed;

package body Carthage.Handles.Units.Configure is

   function To_Weapon_Category
     (Name : String)
      return Weapon_Category;

   -----------------------
   -- Configure_Targets --
   -----------------------

   procedure Configure_Targets
     (Config : Tropos.Configuration)
   is
   begin
      for Unit_Config of Config loop
         declare
            Unit : constant Unit_Category :=
                     Unit_Category'Value (Unit_Config.Config_Name);
         begin
            for Weapon_Config of Unit_Config loop
               declare
                  Weapon : constant Weapon_Category :=
                             Weapon_Category'Value
                               (Weapon_Config.Config_Name);
               begin
                  Weapon_Targets (Weapon, Unit) := True;
               end;
            end loop;
         end;
      end loop;
   end Configure_Targets;

   --------------------
   -- Configure_Unit --
   --------------------

   procedure Configure_Unit
     (Config : Tropos.Configuration)
   is
      Tag : constant String := Config.Config_Name;
      Is_Sceptre : constant Boolean :=
                     Ada.Strings.Fixed.Index (Tag, "sceptre") > 0;
      Is_Noble   : constant Boolean :=
                     Ada.Strings.Fixed.Index (Tag, "noble") > 0;
      Category   : constant Unit_Category :=
                     Unit_Category'Value
                       (Config.Get ("category"));

      function G (Field_Name : String) return Integer
      is (Config.Get (Field_Name, 0));

      function G (Field_Name : String) return Carthage.Quantities.Quantity_Type
      is (Carthage.Quantities.To_Quantity
          (Real (Float'(Config.Get (Field_Name, 0.0)))));

      function G (Field_Name : String) return Boolean
      is (Config.Get (Field_Name));

      Firebirds : constant Integer := G ("maintenance");
      Maintenance : constant Carthage.Money.Money_Type :=
                      (if Firebirds > 0
                       then Carthage.Money.To_Money (Real (Firebirds))
                       else Carthage.Money.Zero);
      Revenue : constant Carthage.Money.Money_Type :=
                  (if Firebirds < 0
                   then Carthage.Money.To_Money (Real (-Firebirds))
                   else Carthage.Money.Zero);

      Unit       : Unit_Record :=
                     Unit_Record'
                       (Tag            => +Tag,
                        Index          => G ("index") + 1,
                        Category       => Category,
                        Is_Sceptre     => Is_Sceptre,
                        Is_Noble       => Is_Noble,
                        Move           => G ("move"),
                        Spot           => G ("spot"),
                        Camouflage     => G ("camoflage"),
                        Agility        => G ("agility"),
                        Armour         => G ("armour"),
                        Psy_Defence    => G ("psy-defence"),
                        Cargo          => G ("cargo"),
                        Can_Be_Cargo   => G ("can-be-cargo"),
                        Combat         => G ("combat"),
                        Maintenance    => Maintenance,
                        Revenue        => Revenue,
                        Credit_Cost    => G ("credit-cost"),
                        Eat            => G ("eat"),
                        Rank           => G ("rank"),
                        Build_Unit     => Null_Unit_Reference,
                        Turns_To_Build => G ("turns-to-build"),
                        Image_Resource => +Config.Get ("icon"),
                        Weapons        => (others => (0, 0)));
   begin

      if Config.Contains ("attack") then
         declare
            Attack : constant Tropos.Configuration :=
                       Config.Child ("attack");
         begin
            for Cat of Attack loop
               declare
                  Category : constant Weapon_Category :=
                               To_Weapon_Category (Cat.Config_Name);
                  Accuracy : constant Natural :=
                               Cat.Get ("accuracy");
                  Strength : constant Natural :=
                               Cat.Get ("strength");
               begin
                  Unit.Weapons (Category) :=
                    Weapon_Record'
                      (Accuracy => Accuracy,
                       Strength => Strength);
               end;
            end loop;
         end;
      end if;

      Create (Unit);

   exception
      when E : others =>
         raise Constraint_Error with "error while configuring "
           & Config.Config_Name & ": "
           & Ada.Exceptions.Exception_Message (E);
   end Configure_Unit;

   ------------------------
   -- To_Weapon_Category --
   ------------------------

   function To_Weapon_Category
     (Name : String)
      return Weapon_Category
   is
   begin
      if Name = "close-space" then
         return Close_Space;
      elsif Name = "direct-space" then
         return Direct_Space;
      elsif Name = "ranged-space" then
         return Ranged_Space;
      else
         return Weapon_Category'Value (Name);
      end if;
   exception
      when others =>
         raise Constraint_Error with
           "unknown weapon category: " & Name;
   end To_Weapon_Category;

end Carthage.Handles.Units.Configure;
