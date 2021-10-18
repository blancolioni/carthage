with Carthage.Handles.Cities;
with Carthage.Handles.Houses;

package Carthage.Managers.Cities is

   type City_Trade_Group is private;

   function New_Trade_Group return City_Trade_Group;

   function Create_City_Manager
     (House  : Carthage.Handles.Houses.House_Handle;
      Group  : City_Trade_Group;
      City   : Carthage.Handles.Cities.City_Handle;
      Ground : Manager_Type)
      return Manager_Type;

private

   type City_Trade_Group_Record;

   type City_Trade_Group is access City_Trade_Group_Record;

end Carthage.Managers.Cities;
