with Ada.Directories;
with Ada.Text_IO;

with WL.Command_Line;
with WL.Localisation;

with Carthage.Calendar;
with Carthage.Configure;
with Carthage.Logging;
with Carthage.Updates;

with Carthage.Handles.Houses;
with Carthage.UI.Gtk_UI;

with Carthage.Managers.Manager;

with Carthage.Game;

with Carthage.Options;
with Carthage.Paths;

with Carthage.Tests;

procedure Carthage.Main is
   Fading_Suns_Scenario : constant Boolean := True;
begin

   Carthage.Tests.Run_Tests;

   Carthage.Logging.Start_Logging ("carthage.log");

   if Ada.Directories.Exists ("options.txt") then
      WL.Command_Line.Load_Defaults ("options.txt");
   else
      WL.Command_Line.Load_Defaults
        (Carthage.Paths.Config_File ("default-options.txt"));
   end if;

   WL.Localisation.Read_Localisation_Directory
     (Language => WL.Localisation.To_Language ("en"),
      Path     => Carthage.Paths.Config_File ("localisation/english"));

   Carthage.Configure.Load_Configuration;
   if Fading_Suns_Scenario then
      Carthage.Configure.Load_Fading_Suns_Scenario;
   else
      Carthage.Configure.Load_Scenario ("standard");
   end if;

   Carthage.Game.New_Game;

   Ada.Text_IO.Put_Line ("setting up first update");
   Carthage.Updates.Before_First_Update;
   Ada.Text_IO.Put_Line ("done");

   Ada.Text_IO.Put_Line ("setting up managers");

   Carthage.Managers.Manager.Start_Managers;

--     declare
--        procedure Create_Manager (House : Carthage.Handles.Houses.House_Handle);
--
--        --------------------
--        -- Create_Manager --
--        --------------------
--
--        procedure Create_Manager (House : Carthage.Handles.Houses.House_Handle) is
--        begin
--           Carthage.Managers.Houses.Create_House_Manager (House);
--        end Create_Manager;
--
--     begin
--        Carthage.Handles.Houses.Scan (Create_Manager'Access);
--     end;

   Ada.Text_IO.Put_Line ("done");

   if Carthage.Options.Wizard_Mode then
      Carthage.UI.Set_Wizard_Mode (True);
   end if;

   if Carthage.Options.Gtk_UI then
      Carthage.Updates.Set_Time_Acceleration (4.0 * 3600.0);
      Carthage.UI.Gtk_UI.Start
        (Carthage.Handles.Houses.Get (Carthage.Options.House));
   else
      for I in 1 .. Natural'Max (Carthage.Options.Update_Count, 1) loop
         for J in 1 .. 24 loop
            Carthage.Updates.Update;
            Carthage.Calendar.Advance (Carthage.Calendar.Hours (1));
         end loop;
      end loop;
   end if;

   Carthage.Logging.Stop_Logging;
end Carthage.Main;
