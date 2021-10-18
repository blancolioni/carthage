package Carthage.Configure is

   procedure Initialize_Configuration;

   procedure Load_Configuration;

   procedure Load_Fading_Suns_Scenario;

   procedure Load_Scenario (Name : String);

   function Fading_Suns_Bin_File
     (Name : String)
      return String;

   function Fading_Suns_Data_File
     (Name : String)
      return String;

   function Fading_Suns_FLC_File
     (Name : String)
      return String;

   function Fading_Suns_Rand_File
     (Name : String)
      return String;

   procedure Load_Standard_Houses;

end Carthage.Configure;
