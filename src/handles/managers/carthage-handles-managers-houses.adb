with Carthage.Handles.Planets;
with Carthage.Handles.Managers.Planets;

package body Carthage.Handles.Managers.Houses is

   type House_Manager_Record is
     new Root_Manager_Record with
      record
         null;
      end record;

   overriding function Name
     (Manager : House_Manager_Record)
      return String
   is (House (Manager).Long_Name & " manager");

   overriding procedure Register
     (Manager : House_Manager_Record);

   overriding procedure On_Activated
     (Manager : in out House_Manager_Record)
   is null;

   --------------------------
   -- Create_House_Manager --
   --------------------------

   procedure Create_House_Manager
     (House : Carthage.Handles.Houses.House_Handle)
   is
      Rec    : House_Manager_Record;
      Handle : Manager_Handle;

      procedure Create_Planet_Manager
        (Planet : Carthage.Handles.Planets.Planet_Handle);

      ---------------------------
      -- Create_Planet_Manager --
      ---------------------------

      procedure Create_Planet_Manager
        (Planet : Carthage.Handles.Planets.Planet_Handle)
      is
      begin
         Carthage.Handles.Managers.Planets.Create_Planet_Manager
           (Planet => Planet,
            House  => House);
      end Create_Planet_Manager;

   begin
      Rec.Initialize
        (Class     => Carthage.Handles.Managers.House,
         Authority => General,
         House     => House);

      Handle := Rec.Create_Manager;

      Carthage.Handles.Planets.For_All_Planets
        (Create_Planet_Manager'Access);

      Handle.Log ("created");
   end Create_House_Manager;

   --------------
   -- Register --
   --------------

   overriding procedure Register
     (Manager : House_Manager_Record)
   is
   begin
      Manager.Save_Manager;
   end Register;

end Carthage.Handles.Managers.Houses;
