with Ada.Containers.Doubly_Linked_Lists;

with Carthage.Handles.Assets;
with Carthage.Handles.Resources;
with Carthage.Handles.Stacks;

with Carthage.Goals.Assets;

with Carthage.Money;
with Carthage.Quantities;
with Carthage.Real_Images;

package body Carthage.Handles.Managers.Assets is

   package Asset_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Asset_Reference);

   type Ground_Asset_Manager_Record is
     new Root_Manager_Record with
      record
         Planet    : Planet_Reference;
         Assets    : Asset_Lists.List;
         Transport : Manager_Reference;
      end record;

   overriding function Name
     (Manager : Ground_Asset_Manager_Record)
      return String
   is (House (Manager).Tag
       & "/" & Carthage.Handles.Planets.Get (Manager.Planet).Tag
       & " asset manager");

   overriding procedure On_Activated
     (Manager : in out Ground_Asset_Manager_Record);

   overriding procedure Register
     (Manager : Ground_Asset_Manager_Record);

   ---------------------------------
   -- Create_Ground_Asset_Manager --
   ---------------------------------

   procedure Create_Ground_Asset_Manager
     (House     : Carthage.Handles.Houses.House_Handle;
      Planet    : Carthage.Handles.Planets.Planet_Handle;
      Transport : Manager_Handle)
   is

      function Get_Planetary_Assets return Asset_Lists.List;

      --------------------------
      -- Get_Planetary_Assets --
      --------------------------

      function Get_Planetary_Assets return Asset_Lists.List is
         List : Asset_Lists.List;

         procedure Add_Assets (Reference : Stack_Reference);

         ----------------
         -- Add_Assets --
         ----------------

         procedure Add_Assets (Reference : Stack_Reference) is
            Stack : constant Carthage.Handles.Stacks.Stack_Handle :=
                      Carthage.Handles.Stacks.Get (Reference);
         begin
            for I in 1 .. Stack.Asset_Count loop
               declare
                  Asset : constant Carthage.Handles.Assets.Asset_Handle :=
                            Carthage.Handles.Assets.Get
                              (Stack.Asset (I));
               begin
                  if Asset.Is_Ground_Asset then
                     List.Append (Asset.Reference);
                  end if;
               end;
            end loop;
         end Add_Assets;

      begin
         Planet.For_All_Owned_Stacks (House.Reference, Add_Assets'Access);
         return List;
      end Get_Planetary_Assets;

      Rec    : Ground_Asset_Manager_Record;
      Handle : Manager_Handle;
   begin
      Rec.Initialize
        (Class        => Ground_Assets,
         House        => House,
         First_Event  => Carthage.Calendar.Days (30),
         Random_Start => False);

      Rec.Planet := Planet.Reference;
      Rec.Transport := Transport.Reference;
      Rec.Assets := Get_Planetary_Assets;

      Handle := Rec.Create_Manager;

      if not Rec.Assets.Is_Empty then
         Handle.Log ("created: asset count" & Rec.Assets.Length'Image);
      end if;
   end Create_Ground_Asset_Manager;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated
     (Manager : in out Ground_Asset_Manager_Record)
   is
      use Carthage.Money, Carthage.Quantities;
      House        : constant Carthage.Handles.Houses.House_Handle :=
                       Carthage.Handles.Houses.Get (Manager.House);
      Food         : constant Carthage.Handles.Resources.Resource_Handle :=
                       Carthage.Handles.Resources.Food;
      Maintenance  : Money_Type := Zero;
      Revenue      : Money_Type := Zero;
      Need_Food    : Quantity_Type := Zero;
      Carried_Food : Quantity_Type := Zero;
      Supply_Asset : Asset_Lists.List;

      procedure Add_Asset
        (Goal     : in out Carthage.Goals.Goal_Record'Class;
         Complete : out Boolean);

      ---------------
      -- Add_Asset --
      ---------------

      procedure Add_Asset
        (Goal     : in out Carthage.Goals.Goal_Record'Class;
         Complete : out Boolean)
      is
      begin
         Manager.Assets.Append
           (Carthage.Goals.Assets.Get_Asset (Goal));
         Complete := True;
      end Add_Asset;

   begin

      Manager.Update_Goals
        (Test     => Carthage.Goals.Assets.Is_Add_Asset_Goal'Access,
         Process  => Add_Asset'Access);

      if not Manager.Assets.Is_Empty then
         for Reference of Manager.Assets loop
            declare
               Asset : constant Carthage.Handles.Assets.Asset_Handle :=
                         Carthage.Handles.Assets.Get (Reference);
            begin
               Maintenance := Maintenance + Asset.Maintenance;
               Revenue := Revenue + Asset.Revenue;
               Need_Food := Need_Food + Asset.Unit.Eat;

               if Asset.Has_Resource (Food) then
                  Carried_Food := Carried_Food + Asset.Resource_Quantity;
                  Supply_Asset.Append (Asset.Reference);
               end if;
            end;
         end loop;

         Revenue := Adjust (Revenue, 1.0 / 12.0);
         Manager.Log ("unit count" & Manager.Assets.Length'Image
                      & ": revenue "
                      & Show (Revenue));
         House.Earn (Revenue);
         House.Log_Status;

         Maintenance := Adjust (Maintenance, 1.0 / 12.0);
         Manager.Log ("unit count" & Manager.Assets.Length'Image
                      & ": base maintenance "
                      & Show (Maintenance)
                      & " x unit pay "
                      & Carthage.Real_Images.Approximate_Image
                        (House.Unit_Pay * 100.0)
                      & "%; total "
                      & Show (Adjust (Maintenance, House.Unit_Pay)));

         House.Spend (Adjust (Maintenance, House.Unit_Pay));
         House.Log_Status;

         if not Manager.First_Update then
            declare
               Have_Food : constant Quantity_Type :=
                             Manager.Quantity (Food);
               Eat_Food  : constant Quantity_Type :=
                             Min (Have_Food + Carried_Food, Need_Food);
               Remaining : Quantity_Type := Eat_Food;
               Provided  : Quantity_Type := Zero;
            begin
               for Reference of Supply_Asset loop
                  declare
                     use Carthage.Handles.Assets;
                     Asset : constant Asset_Handle := Get (Reference);
                     Quantity : constant Quantity_Type :=
                                  Min (Remaining, Asset.Resource_Quantity);
                  begin
                     Provided := Provided + Quantity;
                     Remaining := Remaining - Quantity;
                     Asset.Remove_Quantity (Quantity);
                     exit when Remaining = Zero;
                  end;
               end loop;

               if Remaining > Zero then
                  Manager.Remove (Food, Remaining);
               end if;

               Manager.Log
                 ("food: need " & Show (Need_Food)
                  & "; have " & Show (Have_Food)
                  & "; supplied " & Show (Carried_Food)
                  & "; eat " & Show (Eat_Food)
                  & "; remaining "
                  & Show (Carried_Food - Provided)
                  & "/"
                  & Show (Manager.Quantity (Food)));

            end;
         end if;

         declare
            Want_Food : constant Quantity_Type :=
                          Scale (Need_Food, 1.5);
            Have_Food : constant Quantity_Type :=
                          Manager.Quantity (Food)
                          + Scale (Carried_Food, 0.25);
         begin
            if Have_Food < Want_Food then
               Manager.Log ("asking for "
                            & Show (Want_Food - Have_Food)
                            & " food");
               --  Manager.Add_Pending_Goal
               --    (To_Manager => Get (Manager.Transport),
               --     Goal       =>
               --       Carthage.Goals.Transport.Resource_Request
               --         (Destination => ,
               --          Resource    => Food,
               --          Quantity    => Want_Food - Have_Food,
               --          Priority    => Carthage.Goals.Highest_Priority));
            end if;
         end;

      end if;

      Manager.Schedule_Next_Update
        (Carthage.Calendar.Days (30));

   end On_Activated;

   --------------
   -- Register --
   --------------

   overriding procedure Register
     (Manager : Ground_Asset_Manager_Record)
   is
   begin
      Manager.Save_Manager (Planet => Manager.Planet);
   end Register;
end Carthage.Handles.Managers.Assets;
