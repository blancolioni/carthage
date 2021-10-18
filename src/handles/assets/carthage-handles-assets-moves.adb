with Ada.Containers.Indefinite_Holders;

with WL.Random;

with Reiko.Updates;

with Carthage.Calendar;

with Carthage.Handles.Galaxy;
with Carthage.Handles.Stacks;

package body Carthage.Handles.Assets.Moves is

   package Jump_Route_Holders is
     new Ada.Containers.Indefinite_Holders
       (Carthage.Handles.Planets.Array_Of_Planets,
        Carthage.Handles.Planets."=");

   type Jump_Update is
     new Reiko.Root_Update_Type with
      record
         Asset : Carthage.Handles.Assets.Asset_Handle;
         Path  : Jump_Route_Holders.Holder;
      end record;

   overriding function Name
     (Update : Jump_Update)
      return String
   is ("jump");

   overriding procedure Execute
     (Update : Jump_Update);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Jump_Update)
   is
      Route : constant Carthage.Handles.Planets.Array_Of_Planets :=
                Update.Path.Element;
      Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                 Route (Route'First);
      From_Stack : constant Carthage.Handles.Stacks.Stack_Handle :=
                     Carthage.Handles.Stacks.Get (Update.Asset.Stack);
      To_Stack   : constant Carthage.Handles.Stacks.Stack_Handle :=
                     Carthage.Handles.Stacks.Get
                       (Planet.Orbital_Stack (From_Stack.Owner.Reference));
   begin
      Update.Asset.Log
        ("jumping to " & Planet.Tag);
      From_Stack.Remove_Asset (Update.Asset.Reference);
      To_Stack.Add_Asset (Update.Asset.Reference);
      Planet.Set_Seen_By (From_Stack.Owner.Reference);
      Update.Asset.Finish_Jump;

      if Route'Length > 1 then
         Start_Jump
           (Asset       => Update.Asset,
            Start       => Route (Route'First),
            Destination => Route (Route'First + 1));
      end if;
   end Execute;

   ----------------
   -- Start_Jump --
   ----------------

   procedure Start_Jump
     (Asset       : Asset_Handle;
      Start       : Carthage.Handles.Planets.Planet_Handle;
      Destination : Carthage.Handles.Planets.Planet_Handle)
   is
      Delay_Duration : constant Duration :=
                         Carthage.Calendar.Hours (12)
                         + Duration (WL.Random.Random_Number
                                     (1, 12 * 3600));
      Route          : constant Carthage.Handles.Planets.Array_Of_Planets :=
                         Carthage.Handles.Galaxy.Jump_Route
                           (Start, Destination);
      Update         : constant Jump_Update :=
                         Jump_Update'
                           (Reiko.Root_Update_Type with
                            Asset => Asset,
                            Path  =>
                              Jump_Route_Holders.To_Holder (Route));
   begin
      if Route'Length = 0 then
         Asset.Log ("can't find path from "
                    & Start.Local_Text & " to " & Destination.Local_Text);
      else
         Asset.Start_Jump;
         Reiko.Updates.Add_Update
           (Update, Reiko.Reiko_Duration (Delay_Duration));
      end if;
   end Start_Jump;

   procedure Start_Landing
     (Asset : Asset_Handle;
      Tile  : Carthage.Handles.Tiles.Tile_Handle)
   is null;

   procedure Start_Launch
     (Asset : Asset_Handle)
   is null;

end Carthage.Handles.Assets.Moves;
