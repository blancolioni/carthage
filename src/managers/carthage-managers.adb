with Ada.Numerics.Float_Random;

with Reiko.Updates;

with Carthage.Logging;

package body Carthage.Managers is

   Gen : Ada.Numerics.Float_Random.Generator;

   package Active_Manager_Maps is
     new WL.String_Maps (Manager_Type);

   Active_Managers : Active_Manager_Maps.Map;

   ---------
   -- Add --
   ---------

   procedure Add
     (To       : in out Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Natural)
   is
   begin
      Add (To, Resource, Resource_Quantity (Quantity));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (To       : in out Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Resource_Quantity)
   is
      use Resource_Maps;
      Position : constant Cursor := To.Map.Find (Resource.Tag);
   begin
      if Has_Element (Position) then
         declare
            Q : Resource_Quantity renames To.Map (Position);
         begin
            Q := Q + Quantity;
         end;
      else
         To.Map.Insert (Resource.Tag, Quantity);
      end if;
   end Add;

   --------------
   -- Add_Goal --
   --------------

   procedure Add_Goal
     (Manager : in out Root_Manager_Type'Class;
      Goal    : Carthage.Goals.Goal_Record'Class)
   is
   begin
      Manager.Goals.Insert (Goal.Priority, Goal);
   end Add_Goal;

   -----------------
   -- Add_Manager --
   -----------------

   procedure Add_Manager
     (Manager : not null access Root_Manager_Type'Class)
   is
      Upd : constant Manager_Update :=
              Manager_Update'
                (Reiko.Root_Update_Type with
                 Manager => Manager_Type (Manager));
      First_Update_Delay : constant Duration :=
                             Duration
                               (Float (Manager.Average_Update_Frequency)
                                * (Ada.Numerics.Float_Random.Random (Gen)
                                    + 0.5));
   begin
      Reiko.Updates.Add_Update
        (Upd, Reiko.Reiko_Duration (First_Update_Delay));
      Active_Managers.Insert
        (Manager.Name & "--" & Manager.Target_Id, Upd.Manager);
   end Add_Manager;

   -----------------
   -- Check_Goals --
   -----------------

   procedure Check_Goals
     (Manager : not null access Root_Manager_Type'Class)
   is
   begin
      while not Manager.Goals.Is_Empty loop
         if Manager.Check_Goal (Manager.Goals.First_Element) then
            Manager.Goals.Delete_First;
         else
            exit;
         end if;
      end loop;
   end Check_Goals;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Stock : in out Resource_Stock'Class)
   is
   begin
      Stock.Map.Clear;
   end Clear;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Manager_Update)
   is
      Next_Event_Delay : constant Duration :=
                           Update.Manager.Update;
   begin
      if Next_Event_Delay > 0.0 then
         Reiko.Updates.Add_Update
           (Update, Reiko.Reiko_Duration (Next_Event_Delay));
      end if;
   end Execute;

   -----------------
   -- Get_Manager --
   -----------------

   function Get_Manager
     (Name : String;
      Id   : String)
      return Manager_Type
   is
      Key : constant String := Name & "--" & Id;
   begin
      if Active_Managers.Contains (Key) then
         return Active_Managers (Key);
      else
         return null;
      end if;
   end Get_Manager;

   -------------------------------
   -- Get_Resource_Requirements --
   -------------------------------

   procedure Get_Resource_Requirements
     (Manager : in out Root_Manager_Type;
      Minimum : in out Resource_Stock'Class;
      Desired : in out Resource_Stock'Class)
   is
      pragma Unreferenced (Manager);
   begin
      Minimum.Map.Clear;
      Desired.Map.Clear;
   end Get_Resource_Requirements;

   -----------------------------
   -- Have_Immediate_Capacity --
   -----------------------------

   function Have_Immediate_Capacity
     (Manager : Root_Manager_Type;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Manager, Goal);
   begin
      return False;
   end Have_Immediate_Capacity;

   ---------
   -- Log --
   ---------

   procedure Log
     (Manager : Root_Manager_Type'Class;
      Message : String)
   is
   begin
      Carthage.Logging.Log
        (Manager.Log_Identifier
         & ": "
         & Message);
   end Log;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (To       : in out Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Natural)
   is
   begin
      Remove (To, Resource, Resource_Quantity (Quantity));
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (To       : in out Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Resource_Quantity)
   is
      Q : Resource_Quantity renames To.Map (Resource.Tag);
   begin
      Q := Q - Quantity;
   end Remove;

   ----------------
   -- Scan_Stock --
   ----------------

   procedure Scan_Stock
     (Stock   : Resource_Stock;
      Process : not null access
        procedure (Resource : Carthage.Handles.Resources.Resource_Handle;
                   Quantity : Positive))
   is
   begin
      for Position in Stock.Map.Iterate loop
         declare
            Resource : constant Carthage.Handles.Resources.Resource_Handle :=
                         Carthage.Handles.Resources.Get
                           (Resource_Maps.Key (Position));
            Quantity : constant Natural :=
                         Natural (Float'Truncation
                                  (Float
                                     (Resource_Maps.Element (Position))));
         begin
            if Quantity > 0 then
               Process (Resource, Quantity);
            end if;
         end;
      end loop;
   end Scan_Stock;

   ------------------
   -- Set_Quantity --
   ------------------

   procedure Set_Quantity
     (To       : in out Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Resource_Quantity)
   is
      use Resource_Maps;
      Position : constant Cursor := To.Map.Find (Resource.Tag);
   begin
      if Has_Element (Position) then
         declare
            Q : Resource_Quantity renames To.Map (Position);
         begin
            Q := Quantity;
         end;
      else
         To.Map.Insert (Resource.Tag, Quantity);
      end if;
   end Set_Quantity;

   ------------------------
   -- Transfer_Resources --
   ------------------------

   procedure Transfer_Resources
     (Manager : in out Root_Manager_Type;
      From    : in out Carthage.Handles.Resources.Stock_Interface'Class;
      Max     : Resource_Stock'Class)
   is
      procedure Transfer
        (Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Resource_Quantity);

      --------------
      -- Transfer --
      --------------

      procedure Transfer
        (Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Resource_Quantity)
      is
         Transferred : constant Resource_Quantity :=
                         Resource_Quantity'Min
                           (Quantity, Max.Quantity (Resource));
      begin
         if Transferred > 0.0 then
            Manager.Resources.Add (Resource, Transferred);
            From.Remove (Resource, Transferred);
         end if;
      end Transfer;

   begin
      From.Scan_Stock (Transfer'Access);
   end Transfer_Resources;

   procedure Transfer_Resources
     (Manager : in out Root_Manager_Type;
      From    : Carthage.Handles.Cities.City_Handle;
      Max     : Resource_Stock'Class)
   is
      procedure Transfer
        (Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Resource_Quantity);

      --------------
      -- Transfer --
      --------------

      procedure Transfer
        (Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Resource_Quantity)
      is
         Transferred : constant Natural :=
                         Natural'Min
                           (Natural (Quantity), Max.Whole_Quantity (Resource));
      begin
         if Transferred > 0 then
            Manager.Resources.Add (Resource, Transferred);
            From.Remove (Resource, Transferred);
         end if;
      end Transfer;

   begin
      From.Scan_Stock (Transfer'Access);
   end Transfer_Resources;

end Carthage.Managers;
