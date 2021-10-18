with Carthage.Handles.Houses;
with Carthage.Handles.Planets;

package Carthage.Managers.Planets is

   function Create_Active_Planet_Manager
     (House  : Carthage.Handles.Houses.House_Handle;
      Planet : Carthage.Handles.Planets.Planet_Handle;
      Meta   : Stack_Meta_Manager_Access)
      return Manager_Type;

   function Create_Passive_Planet_Manager
     (House  : Carthage.Handles.Houses.House_Handle;
      Planet : Carthage.Handles.Planets.Planet_Handle;
      Meta   : Stack_Meta_Manager_Access)
      return Manager_Type;

end Carthage.Managers.Planets;
