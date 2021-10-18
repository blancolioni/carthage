with Carthage.Handles.Houses;

package Carthage.Handles.Assets.Create is

   function New_Asset
     (Unit      : Carthage.Handles.Units.Unit_Handle;
      Owner     : Carthage.Handles.Houses.House_Handle;
      Stack     : Stack_Reference;
      XP        : Asset_Experience := Green;
      Loyalty   : Loyalty_Type := Loyalty_Type'Last;
      Health    : Health_Type := Health_Type'Last)
      return Asset_Handle;

end Carthage.Handles.Assets.Create;
