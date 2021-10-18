with WL.Images;

with Carthage.Colors;

package Carthage.UI is

   procedure Load_Resources
     (Save_Image_Resource : not null access
        procedure (Resource_Name : String;
                   Image         : WL.Images.Image_Type'Class));

   function Create_Background_Hex
     (Background_Color : Carthage.Colors.Color_Type)
      return WL.Images.Image_Type'Class;

   procedure Set_Wizard_Mode
     (Enabled : Boolean);

   function Wizard_Mode return Boolean;

private

   Local_Wizard_Mode : Boolean := False;

   function Wizard_Mode return Boolean is (Local_Wizard_Mode);

end Carthage.UI;
