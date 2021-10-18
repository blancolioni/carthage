package Carthage.Handles.Cities.Updates is

   procedure Execute_City_Orders
     (City : City_Handle;
      Manager : not null access City_Manager_Interface'Class);

   procedure Execute_City_Production
     (City    : City_Handle;
      Manager : not null access City_Manager_Interface'Class);

end Carthage.Handles.Cities.Updates;
