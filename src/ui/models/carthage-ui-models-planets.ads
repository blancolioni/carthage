with Carthage.Handles.Houses;
with Carthage.Handles.Planets;

private package Carthage.UI.Models.Planets is

   function Planet_Model
     (House  : Carthage.Handles.Houses.House_Handle;
      Planet : Carthage.Handles.Planets.Planet_Handle)
      return Carthage_Model;

end Carthage.UI.Models.Planets;
