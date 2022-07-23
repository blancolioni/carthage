with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;

with Carthage.Handles.Assets;
--  with Carthage.Handles.Resources;
with Carthage.Handles.Stacks;
with Carthage.Handles.Tiles;

with Carthage.Goals.Assets;
with Carthage.Goals.Transport;

package body Carthage.Handles.Managers.Resources is

   package Asset_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Asset_Reference);

   type Ground_Resource_Manager_Record is
     new Root_Manager_Record with
      record
         Planet : Planet_Reference;
         Pods   : Asset_Lists.List;
      end record;

   overriding function Name
     (Manager : Ground_Resource_Manager_Record)
      return String
   is (House (Manager).Tag
       & "/" & Carthage.Handles.Planets.Get (Manager.Planet).Tag
       & " resource manager");

   overriding procedure On_Activated
     (Manager : in out Ground_Resource_Manager_Record);

   overriding procedure Register
     (Manager : Ground_Resource_Manager_Record);

   function Find_Pod
     (Manager     : Ground_Resource_Manager_Record'Class;
      Cargo       : Carthage.Handles.Resources.Resource_Handle;
      Quantity    : Carthage.Quantities.Quantity_Type;
      Destination : Carthage.Handles.Tiles.Tile_Handle)
      return Carthage.Handles.Assets.Asset_Handle;

   --  procedure Move_Pod
   --    (Manager     : Ground_Resource_Manager_Record'Class;
   --     Pod         : Carthage.Handles.Assets.Asset_Handle;
   --     Destination : Carthage.Handles.Tiles.Tile_Handle);

   ---------------------------------
   -- Create_Ground_Resource_Manager --
   ---------------------------------

   procedure Create_Ground_Resource_Manager
     (House     : Carthage.Handles.Houses.House_Handle;
      Planet    : Carthage.Handles.Planets.Planet_Handle)
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
                  if Asset.Unit.Is_Cargo_Pod then
                     List.Append (Asset.Reference);
                  end if;
               end;
            end loop;
         end Add_Assets;

      begin
         Planet.For_All_Owned_Stacks (House.Reference, Add_Assets'Access);
         return List;
      end Get_Planetary_Assets;

      Rec    : Ground_Resource_Manager_Record;
      Handle : Manager_Handle;
   begin
      Rec.Initialize
        (Class        => Ground,
         Authority    => Resource_Management,
         House        => House);

      Rec.Planet := Planet.Reference;
      Rec.Pods := Get_Planetary_Assets;

      Handle := Rec.Create_Manager;

      if not Rec.Pods.Is_Empty then
         Handle.Log ("created: cargo pod count" & Rec.Pods.Length'Image);
      end if;
   end Create_Ground_Resource_Manager;

   --------------
   -- Find_Pod --
   --------------

   function Find_Pod
     (Manager     : Ground_Resource_Manager_Record'Class;
      Cargo       : Carthage.Handles.Resources.Resource_Handle;
      Quantity    : Carthage.Quantities.Quantity_Type;
      Destination : Carthage.Handles.Tiles.Tile_Handle)
      return Carthage.Handles.Assets.Asset_Handle
   is
      use type Carthage.Handles.Resources.Resource_Handle;
      Distance : Natural := Natural'Last;
      Result   : Carthage.Handles.Assets.Asset_Handle :=
                   Carthage.Handles.Assets.Empty_Handle;
      Planet   : constant Carthage.Handles.Planets.Planet_Handle :=
                   Carthage.Handles.Planets.Get (Manager.Planet);
   begin
      for Pod of Manager.Pods loop
         declare
            use type Carthage.Quantities.Quantity_Type;
            Asset : constant Carthage.Handles.Assets.Asset_Handle :=
                      Carthage.Handles.Assets.Get (Pod);
            Stack : constant Carthage.Handles.Stacks.Stack_Handle :=
                      Carthage.Handles.Stacks.Get (Asset.Stack);
         begin
            if not Stack.Has_Movement
              and then Asset.Resource_Cargo = Cargo
              and then Asset.Resource_Quantity >= Quantity
            then
               declare
                  D : constant Natural :=
                        Planet.Hex_Distance
                          (Stack.Current_Tile.Position,
                           Destination.Position);
               begin
                  if D < Distance then
                     Distance := D;
                     Result := Asset;
                  end if;
               end;
            end if;
         end;
      end loop;
      return Result;
   end Find_Pod;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated
     (Manager : in out Ground_Resource_Manager_Record)
   is

      procedure Update_Goal
        (Goal     : in out Carthage.Goals.Goal_Record'Class;
         Complete : out Boolean);

      -----------------
      -- Update_Goal --
      -----------------

      procedure Update_Goal
        (Goal     : in out Carthage.Goals.Goal_Record'Class;
         Complete : out Boolean)
      is
         use Carthage.Goals.Assets, Carthage.Goals.Transport;
      begin
         if Is_Add_Asset_Goal (Goal) then
            Carthage.Handles.Assets.Get (Get_Asset (Goal))
              .Log ("added to " & Manager.Name);
            Manager.Pods.Append (Get_Asset (Goal));
            Complete := True;
         elsif Is_Resource_Request_Goal (Goal) then
            Manager.Log
              ("transport "
               & Carthage.Quantities.Show (Get_Quantity (Goal))
               & " "
               & Get_Resource (Goal).Tag
               & " to "
               & Get_Resource_Destination (Goal).Description);

            declare
               Pod : constant Carthage.Handles.Assets.Asset_Handle :=
                       Manager.Find_Pod
                         (Cargo       => Get_Resource (Goal),
                          Quantity    => Get_Quantity (Goal),
                          Destination => Get_Resource_Destination (Goal));
            begin
               if Pod.Has_Element then
                  declare
                     Stack : constant Carthage.Handles.Stacks.Stack_Handle :=
                               Carthage.Handles.Stacks.Get (Pod.Stack);
                  begin
                     Stack.Move_To_Tile (Get_Resource_Destination (Goal));
                  end;

                  Complete := True;
               end if;
            end;
         end if;
      end Update_Goal;

   begin
      Manager.Log ("activated");
      Manager.Update_Goals (Update_Goal'Access);

   exception
      when E : others =>
         Manager.Log ("exception during activation: "
                      & Ada.Exceptions.Exception_Message (E));
   end On_Activated;

   --------------
   -- Register --
   --------------

   overriding procedure Register
     (Manager : Ground_Resource_Manager_Record)
   is
   begin
      Manager.Save_Manager (Planet => Manager.Planet);
   end Register;
end Carthage.Handles.Managers.Resources;
