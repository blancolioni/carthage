package Carthage.Identifiers is

   function New_Identifier
     (Template : String)
     return String;

   function New_Identifier
     (Template   : String;
      Class_Name : String)
      return String;

end Carthage.Identifiers;
