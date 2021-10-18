with Ada.Containers.Doubly_Linked_Lists;

with Carthage.Handles.Assets;
--  with Carthage.Handles.Resources;
with Carthage.Handles.Stacks;

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
        (Class        => Ground_Resources,
         House        => House);

      Rec.Planet := Planet.Reference;
      Rec.Pods := Get_Planetary_Assets;

      Handle := Rec.Create_Manager;

      if not Rec.Pods.Is_Empty then
         Handle.Log ("created: cargo pod count" & Rec.Pods.Length'Image);
      end if;
   end Create_Ground_Resource_Manager;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated
     (Manager : in out Ground_Resource_Manager_Record)
   is null;

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
