with Ada.Strings.Unbounded;

with Carthage.Handles.Planets;
with Carthage.Handles.Resources;

with Carthage.Handles.Vectors;

package body Carthage.Handles.Houses is

   package House_Vectors is
     new Carthage.Handles.Vectors
       (Real_House_Reference, House_Record, "house");

   package House_Maps is
     new WL.String_Maps (House_Reference);

   House_Vector : House_Vectors.Vector;
   House_Map    : House_Maps.Map;

   function Get
     (Handle : House_Handle)
      return House_Vectors.Constant_Reference_Type
   is (House_Vector (Handle.Reference));

   overriding function Tag
     (This : House_Handle)
      return String
   is (-Get (This).Tag);

   function Category
     (This : House_Handle)
      return House_Category
   is (Get (This).Category);

   function At_War_With
     (This  : House_Handle;
      Other : House_Handle)
      return Boolean
   is (Treaty_Status_With (This, Other) = War);

   function Capital
     (This : House_Handle)
      return Planet_Reference
   is (Get (This).Capital);

   function Cash
     (This : House_Handle)
      return Carthage.Money.Money_Type
   is (Get (This).Cash);

   function Color
     (This : House_Handle)
      return Carthage.Colors.Color_Type
   is (Get (This).Color);

   function Debt
     (This : House_Handle)
      return Carthage.Money.Money_Type
   is (Get (This).Debt);

   function Unit_Pay
     (This : House_Handle)
      return Unit_Real
   is (Get (This).Unit_Pay);

   function Get (Tag : String) return House_Handle
   is (if House_Map.Contains (Tag)
       then Get (House_Map (Tag))
       else (raise Constraint_Error with
           "no such house: " & Tag));

   procedure Update_Resource
     (House    : House_Handle;
      Planet   : Planet_Reference;
      Resource : Resource_Reference;
      Change   : Resource_Record);

   type House_Manager_Reference is
     access all House_Manager_Interface'Class;

   package Manager_Maps is
     new WL.String_Maps (House_Manager_Reference);

   Manager_Map : Manager_Maps.Map;

   ----------------------
   -- Add_Known_Planet --
   ----------------------

   procedure Add_Known_Planet
     (This   : House_Handle;
      Planet : Planet_Reference)
   is
      procedure Update (Rec : in out House_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out House_Record) is
      begin
         Rec.Known_Planets.Append (Planet);
      end Update;

   begin
      House_Vector.Update (This.Reference, Update'Access);
   end Add_Known_Planet;

   ----------------------
   -- Consume_Resource --
   ----------------------

   procedure Consume_Resource
     (House    : House_Handle;
      Planet   : Planet_Reference;
      Resource : Resource_Reference;
      Quantity : Carthage.Quantities.Quantity_Type)
   is
   begin
      Update_Resource (House, Planet, Resource,
                       Resource_Record'
                         (Consumed => Quantity, others => <>));
   end Consume_Resource;

   ------------
   -- Create --
   ------------

   function Create (Rec : House_Record) return House_Reference is
      Reference : House_Reference;
   begin
      House_Vector.Append (Rec, Reference);
      House_Map.Insert (-Rec.Tag, Reference);
      return Reference;
   end Create;

   ----------
   -- Earn --
   ----------

   procedure Earn
     (This   : House_Handle;
      Amount : Carthage.Money.Money_Type)
   is
      use type Carthage.Money.Money_Type;

      procedure Update (Rec : in out House_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out House_Record) is
      begin
         Rec.Cash := Rec.Cash + Amount;
      end Update;

   begin
      House_Vector.Update (This.Reference, Update'Access);
   end Earn;

   -------------
   -- Exclude --
   -------------

   procedure Exclude
     (House : House_Handle;
      Set   : in out Set_Of_Houses)
   is
   begin
      Exclude (Set, House.Reference);
   end Exclude;

   --------------------
   -- For_All_Houses --
   --------------------

   procedure For_All_Houses
     (Process : not null access
        procedure (House : House_Handle))
   is
   begin
      for Reference in 1 .. House_Vector.Last_Index loop
         Process (Get (Reference));
      end loop;
   end For_All_Houses;

   -------------
   -- Include --
   -------------

   procedure Include
     (House : House_Handle;
      Set   : in out Set_Of_Houses)
   is
   begin
      Include (Set, House.Reference);
   end Include;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      House_Vector.Read (Stream);
      for Reference in 1 .. House_Vector.Last_Index loop
         House_Map.Insert (Get (Reference).Tag, Reference);
      end loop;
   end Load;

   ----------------
   -- Log_Status --
   ----------------

   procedure Log_Status
     (This : House_Handle)
   is
      Rec : House_Record renames Get (This);
   begin
      This.Log
        ("cash: " & Carthage.Money.Show (Cash (This))
         & "; debt: " & Carthage.Money.Show (Debt (This)));
      for Planet_Position in Rec.Current_Resources.Iterate loop
         declare
            Planet_Tag : constant String :=
                           Planet_Resource_Maps.Key (Planet_Position);
         begin
            for Resource_Position in
              Rec.Current_Resources (Planet_Position).Iterate
            loop
               declare
                  Resource_Tag : constant String :=
                                   Resource_Maps.Key (Resource_Position);
                  Resource_Rec : constant Resource_Record :=
                                   Resource_Maps.Element (Resource_Position);
               begin
                  This.Log
                    (Planet_Tag & "/" & Resource_Tag
                     & ": produced "
                     & Carthage.Quantities.Show (Resource_Rec.Produced)
                     & "; consumed "
                     & Carthage.Quantities.Show (Resource_Rec.Consumed));
               end;
            end loop;
         end;
      end loop;
   end Log_Status;

   ----------------------
   -- Produce_Resource --
   ----------------------

   procedure Produce_Resource
     (House    : House_Handle;
      Planet   : Planet_Reference;
      Resource : Resource_Reference;
      Quantity : Carthage.Quantities.Quantity_Type)
   is
   begin
      Update_Resource (House, Planet, Resource,
                       Resource_Record'
                         (Produced => Quantity, others => <>));
   end Produce_Resource;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      House_Vector.Write (Stream);
   end Save;

   ------------------
   -- Save_History --
   ------------------

   procedure Save_History (This : House_Handle'Class) is

      procedure Update (Rec : in out House_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out House_Record) is
      begin
         Rec.Resource_History.Append (Rec.Current_Resources);
         Rec.Current_Resources.Clear;
      end Update;

   begin
      House_Vector.Update (This.Reference, Update'Access);
   end Save_History;

   ------------------------
   -- Scan_Known_Planets --
   ------------------------

   procedure Scan_Known_Planets
     (This    : House_Handle;
      Process : not null access
        procedure (Planet : Planet_Reference))
   is
   begin
      for Planet of Get (This).Known_Planets loop
         Process (Planet);
      end loop;
   end Scan_Known_Planets;

   -----------------------
   -- Set_House_Manager --
   -----------------------

   procedure Set_House_Manager
     (This    : House_Handle;
      Manager : not null access House_Manager_Interface'Class)
   is
   begin
      Manager_Map.Insert (Tag (This), House_Manager_Reference (Manager));
   end Set_House_Manager;

   -------------------------
   -- Set_Initial_Capital --
   -------------------------

   procedure Set_Initial_Capital
     (Handle  : House_Handle'Class;
      Capital : Planet_Reference)
   is
      procedure Update (Rec : in out House_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out House_Record) is
      begin
         Rec.Capital := Capital;
      end Update;

   begin
      House_Vector.Update (Handle.Reference, Update'Access);
   end Set_Initial_Capital;

   -----------
   -- Spend --
   -----------

   procedure Spend
     (This   : House_Handle;
      Amount : Carthage.Money.Money_Type)
   is
      use type Carthage.Money.Money_Type;

      procedure Update (Rec : in out House_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out House_Record) is
      begin
         if Amount <= Rec.Cash then
            Rec.Cash := Rec.Cash - Amount;
         else
            Rec.Debt := Rec.Debt + Amount - Rec.Cash;
            Rec.Cash := Carthage.Money.Zero;
         end if;
      end Update;

   begin
      House_Vector.Update (This.Reference, Update'Access);
   end Spend;

   ------------------------
   -- Treaty_Status_With --
   ------------------------

   function Treaty_Status_With
     (This  : House_Handle;
      Other : House_Handle)
      return Treaty_Status
   is
   begin
      return Get (This).Treaties (Other.Reference);
   end Treaty_Status_With;

   procedure Update_Resource
     (House    : House_Handle;
      Planet   : Planet_Reference;
      Resource : Resource_Reference;
      Change   : Resource_Record)
   is
      Planet_Tag : constant String :=
                     Carthage.Handles.Planets.Get (Planet).Tag;

      Resource_Tag : constant String :=
                       Carthage.Handles.Resources.Get (Resource).Tag;

      procedure Update (Rec : in out House_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out House_Record) is
      begin
         if not Rec.Current_Resources.Contains (Planet_Tag) then
            Rec.Current_Resources.Insert
              (Planet_Tag, Resource_Maps.Empty_Map);
         end if;

         declare
            use Carthage.Quantities;
            Resources : Resource_Maps.Map renames
                          Rec.Current_Resources (Planet_Tag);
         begin
            if not Resources.Contains (Resource_Tag) then
               Resources.Insert
                 (Resource_Tag, Change);
            else
               declare
                  Res : Resource_Record renames Resources (Resource_Tag);
               begin
                  Res.Consumed := Res.Consumed + Change.Consumed;
                  Res.Produced := Res.Produced + Change.Produced;
               end;
            end if;
         end;

      end Update;

   begin
      House_Vector.Update (House.Reference, Update'Access);
   end Update_Resource;

end Carthage.Handles.Houses;
