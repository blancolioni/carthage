with Ada.Containers.Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

--  with Carthage.Options;

with Carthage.Calendar;

with Carthage.Handles.Assets;
with Carthage.Handles.Cities;
with Carthage.Handles.Galaxy;
with Carthage.Handles.Goals;
with Carthage.Handles.Houses;
with Carthage.Handles.Managers;
with Carthage.Handles.Planets;
with Carthage.Handles.Resources;
with Carthage.Handles.Stacks;
with Carthage.Handles.Stocks;
with Carthage.Handles.Structures;
with Carthage.Handles.Technology;
with Carthage.Handles.Terrain;
with Carthage.Handles.Tiles;
with Carthage.Handles.Units;
with Carthage.Handles.Worlds;

package body Carthage.Handles.State is

   type Handle_Streamer is
   not null access procedure
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   type Handle_Class_Record is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Load : Handle_Streamer;
         Save : Handle_Streamer;
      end record;

   package Handle_Class_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Handle_Class_Record);

   Handle_Classes : Handle_Class_Lists.List;

   function State_Path return String;

   ----------------
   -- Load_State --
   ----------------

   procedure Load_State is
      use Ada.Streams.Stream_IO;
      File   : File_Type;
   begin
      Open (File, In_File, State_Path);
      declare
         S : constant Stream_Access := Stream (File);
      begin
         Object_Identifier'Read (S, Current_Identifier);

         declare
            Clock : Carthage.Calendar.Time;
         begin
            Carthage.Calendar.Time'Read (S, Clock);
            Carthage.Calendar.Set_Clock (Clock);
         end;

         for Element of Handle_Classes loop
            Element.Load (S);
         end loop;

      end;
      Close (File);
   end Load_State;

   ---------------
   -- New_State --
   ---------------

   procedure New_State is
   begin
      Current_Identifier := "0AA00AA0";
   end New_State;

   ----------------
   -- Save_State --
   ----------------

   procedure Save_State is
      use Ada.Streams.Stream_IO;
      File : File_Type;
   begin

      Ada.Text_IO.Put ("Saving state ... ");
      Ada.Text_IO.Flush;

      Create (File, Out_File, State_Path);
      declare
         S : constant Stream_Access := Stream (File);
      begin
         Object_Identifier'Write (S, Current_Identifier);
         Carthage.Calendar.Time'Write (S, Carthage.Calendar.Clock);

         for Element of Handle_Classes loop
            Element.Save (S);
         end loop;

      end;
      Close (File);
      Ada.Text_IO.Put_Line ("done");
   end Save_State;

   ----------------
   -- State_Path --
   ----------------

   function State_Path return String is
      Games_Path   : constant String := "games";
      --  Carthage.Options.Game_Folder;
      Game_Id_Path : constant String :=
                       Games_Path & "/" & "test-game";
                       --  & Carthage.Options.Game_Id;
      File_Name    : constant String := "state.carthage";
   begin
      if not Ada.Directories.Exists (Games_Path) then
         Ada.Directories.Create_Directory (Games_Path);
      end if;
      if not Ada.Directories.Exists (Game_Id_Path) then
         Ada.Directories.Create_Directory (Game_Id_Path);
      end if;
      return Game_Id_Path & "/" & File_Name;
   end State_Path;

begin
   declare
      procedure Add
        (Name       : String;
         Load, Save : Handle_Streamer);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name       : String;
         Load, Save : Handle_Streamer)
      is
      begin
         Handle_Classes.Append
           (Handle_Class_Record'
              (Name => +Name,
               Load => Load,
               Save => Save));
      end Add;

   begin
      Add ("assets", Assets.Load'Access, Assets.Save'Access);
      Add ("cities", Cities.Load'Access, Cities.Save'Access);
      Add ("galaxy", Galaxy.Load'Access, Galaxy.Save'Access);
      Add ("goals", Goals.Load'Access, Goals.Save'Access);
      Add ("houses", Houses.Load'Access, Houses.Save'Access);
      Add ("managers", Managers.Load'Access, Managers.Save'Access);
      Add ("planets", Planets.Load'Access, Planets.Save'Access);
      Add ("resources", Resources.Load'Access, Resources.Save'Access);
      Add ("stacks", Stacks.Load'Access, Stacks.Save'Access);
      Add ("stocks", Stocks.Load'Access, Stocks.Save'Access);
      Add ("structures", Structures.Load'Access, Structures.Save'Access);
      Add ("technology", Technology.Load'Access, Technology.Save'Access);
      Add ("terrain", Terrain.Load'Access, Terrain.Save'Access);
      Add ("tiles", Tiles.Load'Access, Tiles.Save'Access);
      Add ("units", Units.Load'Access, Units.Save'Access);
      Add ("worlds", Worlds.Load'Access, Worlds.Save'Access);
   end;

end Carthage.Handles.State;
