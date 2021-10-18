with Ada.Calendar;
with Ada.Directories;
with Ada.Text_IO;

with WL.Command_Line;
with WL.Processes;

with Reiko.Control;

with Carthage.Logging;
with Carthage.Options;
with Carthage.Paths;
with Carthage.Real_Images;

with Carthage.Configure;

with Carthage.Handles.State;

with Carthage.Handles.Cities;
with Carthage.Handles.Houses;
with Carthage.Handles.Managers.Houses;
with Carthage.Handles.Managers.Updates;
with Carthage.Handles.Stacks.Updates;

with Carthage.Calendar;
with Carthage.UI.Gtk_UI;

procedure Carthage.Driver is
   Master_Options_Path  : constant String :=
                            Carthage.Paths.Config_File ("default-options.txt");
   Local_Options_Path   : constant String :=
                            ".carthage-options";
   Fading_Suns_Scenario : constant Boolean := True;
   Updates_Running      : Boolean := False;
begin

   if not Ada.Directories.Exists (Local_Options_Path) then
      if Ada.Directories.Exists (Master_Options_Path) then
         Ada.Directories.Copy_File (Master_Options_Path, Local_Options_Path);
      else
         raise Constraint_Error with "cannot find configuration";
      end if;
   end if;

   WL.Command_Line.Load_Defaults (Local_Options_Path);

   Carthage.Configure.Initialize_Configuration;

   if Carthage.Options.Create then

      Carthage.Logging.Start_Logging ("carthage-create.log");

      Carthage.Handles.State.New_State;

      Carthage.Configure.Load_Configuration;
      if Fading_Suns_Scenario then
         Carthage.Configure.Load_Fading_Suns_Scenario;
      else
         Carthage.Configure.Load_Scenario ("standard");
      end if;

      Ada.Text_IO.Put ("Initializing visibility ... ");
      Ada.Text_IO.Flush;

      Carthage.Handles.Cities.For_All_Cities
        (Carthage.Handles.Cities.Look'Access);
      Carthage.Handles.Stacks.For_All_Stacks
        (Carthage.Handles.Stacks.Updates.Look'Access);
      Ada.Text_IO.Put_Line ("done");

      Ada.Text_IO.Put ("Creating managers ... ");
      Ada.Text_IO.Flush;
      Carthage.Handles.Houses.For_All_Houses
        (Carthage.Handles.Managers.Houses.Create_House_Manager'Access);
      Ada.Text_IO.Put_Line ("done");

      Carthage.Handles.State.Save_State;

      Carthage.Logging.Stop_Logging;

   elsif Carthage.Options.Report_State then
      Carthage.Handles.State.Load_State;

      declare
         procedure Show_House
           (House : Carthage.Handles.Houses.House_Handle);

         ----------------
         -- Show_House --
         ----------------

         procedure Show_House
           (House : Carthage.Handles.Houses.House_Handle)
         is
         begin
            Ada.Text_IO.Put_Line
              (House.Long_Name);
         end Show_House;

      begin
         Carthage.Handles.Houses.For_All_Houses
           (Show_House'Access);
      end;

   else

      Carthage.Logging.Start_Logging ("carthage.log");

      Ada.Text_IO.Put ("Loading state ...");
      Ada.Text_IO.Flush;

      Carthage.Handles.State.Load_State;

      Ada.Text_IO.Put_Line ("done");

      Reiko.Control.Start
        (Current_Time =>
           Reiko.Reiko_Time
             (Carthage.Calendar.To_Days (Carthage.Calendar.Clock)),
         Task_Count   => Natural'Max (Carthage.Options.Task_Count, 1));

      Updates_Running := True;

      Carthage.Handles.Managers.Updates.Start_Managers;

      if Carthage.Options.Wizard_Mode then
         Carthage.UI.Set_Wizard_Mode (True);
      end if;

      if Carthage.Options.Gtk_UI then
         Carthage.UI.Gtk_UI.Start
           (Carthage.Handles.Houses.Get (Carthage.Options.House));
      elsif Carthage.Options.Update then

         Ada.Text_IO.Put_Line ("tasks:" & Carthage.Options.Task_Count'Image);
         Ada.Text_IO.Put_Line ("days: " & Carthage.Options.Update_Count'Image);

         declare
            Process : WL.Processes.Process_Type;
            Day_Count : constant Positive :=
                          Natural'Max (Carthage.Options.Update_Count, 1);
            Reiko_Minute : constant := 1.0 / 24.0 / 60.0;
            Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         begin
            Process.Start_Bar
              (Name            => "Updating",
               Finish          => Day_Count,
               With_Percentage => False,
               Bar_Length      => 40,
               Tick_Size       => 1);

            for I in 1 .. Day_Count loop
               for J in 1 .. 24 loop
                  for K in 1 .. 60 loop
                     Carthage.Calendar.Advance (60.0);
                     Reiko.Control.Advance (Reiko_Minute);
                  end loop;
               end loop;
               Process.Tick;
            end loop;
            Process.Finish;

            declare
               use Ada.Calendar;
               Elapsed : constant Duration := Clock - Start;
            begin
               Ada.Text_IO.Put_Line
                 ("Advanced" & Day_Count'Image & " days in "
                  & Carthage.Real_Images.Approximate_Image
                    (Real (Elapsed))
                  & "s");
            end;
         end;
      end if;

      Reiko.Control.Stop;
      Updates_Running := False;

      Carthage.Handles.State.Save_State;
      Carthage.Logging.Stop_Logging;

   end if;

exception
   when others =>
      if Updates_Running then
         Reiko.Control.Stop;
      end if;

      --  Carthage.Handles.State.Save_State;

      Carthage.Logging.Stop_Logging;
      raise;
end Carthage.Driver;
