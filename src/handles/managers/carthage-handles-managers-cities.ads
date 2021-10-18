with Carthage.Handles.Cities;
with Carthage.Handles.Houses;

package Carthage.Handles.Managers.Cities is

   procedure Create_City_Manager
     (House            : Carthage.Handles.Houses.House_Handle;
      City             : Carthage.Handles.Cities.City_Handle;
      Resource_Manager : Manager_Handle);

end Carthage.Handles.Managers.Cities;
