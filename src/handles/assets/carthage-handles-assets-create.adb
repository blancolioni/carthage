with Carthage.Goals.Assets;

with Carthage.Handles.Managers;
with Carthage.Handles.Stacks;

package body Carthage.Handles.Assets.Create is

   ---------------
   -- New_Asset --
   ---------------

   function New_Asset
     (Unit      : Carthage.Handles.Units.Unit_Handle;
      Owner     : Carthage.Handles.Houses.House_Handle;
      Stack     : Stack_Reference;
      XP        : Asset_Experience := Green;
      Loyalty   : Loyalty_Type := Loyalty_Type'Last;
      Health    : Health_Type := Health_Type'Last)
      return Asset_Handle
   is
      Rec : constant Asset_Record := Asset_Record'
        (Identifier => Next_Identifier,
         Owner      => Owner.Reference,
         Unit       => Unit.Reference,
         Stack      => Stack,
         Resource   => Null_Resource_Reference,
         Quantity   => Carthage.Quantities.Zero,
         Active     => True,
         Health     => Health,
         Loyalty    => Loyalty,
         Experience => XP,
         Jumping    => False,
         Launching  => False,
         Landing    => False);
      Ref : constant Asset_Reference := Create_Asset (Rec);
   begin
      Carthage.Handles.Stacks.Get (Stack).Add_Asset (Ref);

      if Unit.Is_Ground_Unit then
         declare
            Manager : constant Carthage.Handles.Managers.Manager_Handle :=
                        Carthage.Handles.Managers.Get_Manager
                          (Class  => Carthage.Handles.Managers.Ground_Assets,
                           House  => Owner.Reference,
                           Planet =>
                             Carthage.Handles.Stacks.Get (Stack).Planet);
         begin
            if Manager.Has_Element then
               Carthage.Handles.Managers.Add_Pending_Goal
                 (To_Manager => Manager,
                  Goal       =>
                    Carthage.Goals.Assets.Add_Asset_To_Manager
                      (Ref));
            end if;
         end;

         if Unit.Is_Cargo_Pod then
            declare
               Manager : constant Carthage.Handles.Managers.Manager_Handle :=
                           Carthage.Handles.Managers.Get_Manager
                             (Class  => Handles.Managers.Ground_Resources,
                              House  => Owner.Reference,
                              Planet =>
                                Carthage.Handles.Stacks.Get (Stack).Planet);
            begin
               if Manager.Has_Element then
                  Carthage.Handles.Managers.Add_Pending_Goal
                    (To_Manager => Manager,
                     Goal       =>
                       Carthage.Goals.Assets.Add_Asset_To_Manager
                         (Ref));
               end if;
            end;
         end if;
      end if;

      return Get (Ref);
   end New_Asset;

end Carthage.Handles.Assets.Create;
