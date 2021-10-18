with Ada.Text_IO;

with Carthage.Handles.Houses;
with Carthage.Managers.Houses;

package body Carthage.Managers.Manager is

   procedure Start_House_Manager
     (House : Carthage.Handles.Houses.House_Handle);

   -------------------------
   -- Start_House_Manager --
   -------------------------

   procedure Start_House_Manager
     (House : Carthage.Handles.Houses.House_Handle)
   is
   begin
      Carthage.Managers.Houses.Create_House_Manager (House);
   end Start_House_Manager;

   --------------------
   -- Start_Managers --
   --------------------

   procedure Start_Managers is
   begin
      Ada.Text_IO.Put ("starting managers:");
      Ada.Text_IO.Flush;

      declare
         procedure Start (House : Carthage.Handles.Houses.House_Handle);

         -----------
         -- Start --
         -----------

         procedure Start (House : Carthage.Handles.Houses.House_Handle) is
         begin
            Ada.Text_IO.Put (" " & House.Tag);
            Ada.Text_IO.Flush;
            Start_House_Manager (House);
         end Start;

      begin
         Carthage.Handles.Houses.For_All_Houses (Start'Access);
      end;

      Ada.Text_IO.New_Line;
   end Start_Managers;

end Carthage.Managers.Manager;
