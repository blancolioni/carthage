with Ada.Text_IO;

with Glib;
with Glib.Error;
with Glib.Object;

with Gtk.Builder;
with Gtk.Button;
with Gtk.Cell_Renderer_Text;
with Gtk.Container;
with Gtk.Main;
with Gtk.Tree_Model;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Widget;
with Gtk.Window;

with Lui.Models;
with Lui.Rendering;

with Lui.Gtk_UI;

with Carthage.Paths;
with Carthage.UI.Models.Top;

--  with Carthage.Calendar;
--  with Carthage.Updates;

package body Carthage.UI.Gtk_UI is

   type Carthage_UI is
     new Glib.Object.GObject_Record
     and Lui.Gtk_UI.Lui_Gtk_Interface with
      record
         Models      : Lui.Models.Active_Model_List;
         Model_Area  : Gtk.Container.Gtk_Container;
         Active_Area : Gtk.Widget.Gtk_Widget;
      end record;

   type Carthage_UI_Access is access all Carthage_UI'Class;

   overriding procedure Append_Feature
     (To      : in out Carthage_UI;
      Feature : Lui.Lui_UI_Feature;
      Element : not null access Lui.Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   overriding procedure Select_Feature
     (To      : in out Carthage_UI;
      Feature : Lui.Lui_UI_Feature;
      Element : not null access Lui.Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   overriding procedure Clear_Features
     (To      : in out Carthage_UI;
      Feature : Lui.Lui_UI_Feature);

--     overriding procedure On_Idle
--       (State : in out Carthage_UI);

   procedure Create_Property_List
     (Tree_View : Gtk.Tree_View.Gtk_Tree_View);
   pragma Unreferenced (Create_Property_List);

   procedure Show_Properties
     (UI      : Carthage_UI'Class;
      Model   : Lui.Models.Object_Model);

   procedure Destroy_Handler (W : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Step_Button_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class)
     with Unreferenced;

--     procedure On_Switch_Page
--       (Self     : access Glib.Object.GObject_Record'Class;
--        Page     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
--        Page_Num : Glib.Guint);

   --------------------
   -- Append_Feature --
   --------------------

   overriding procedure Append_Feature
     (To      : in out Carthage_UI;
      Feature : Lui.Lui_UI_Feature;
      Element : not null access Lui.Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use Lui;
      use type Gtk.Widget.Gtk_Widget;
   begin
      case Feature is
         when UI_Gadget =>
            null;
         when UI_Model =>
            declare
               Model : constant Lui.Models.Object_Model :=
                         Lui.Models.Object_Model (Element);
            begin
               To.Models.Append (Model);
               if To.Active_Area /= null then
                  To.Active_Area.Ref;
                  To.Model_Area.Remove (To.Active_Area);
               end if;

               To.Model_Area.Add (Top);
               To.Active_Area := Gtk.Widget.Gtk_Widget (Top);
               To.Show_Properties (Model);
            end;
         when UI_Table =>
            null;
--              To.Info_Boxes.Add (Top);
      end case;
   end Append_Feature;

   --------------------
   -- Clear_Features --
   --------------------

   overriding procedure Clear_Features
     (To      : in out Carthage_UI;
      Feature : Lui.Lui_UI_Feature)
   is
      pragma Unreferenced (To);
      use Lui;
   begin
      case Feature is
         when UI_Gadget =>
            null;
         when UI_Model =>
            null;
         when UI_Table =>
            null;
--              while To.Info_Boxes.Get_Child (0) /= null loop
--                 To.Info_Boxes.Remove (To.Info_Boxes.Get_Child (0));
--              end loop;
      end case;
   end Clear_Features;

   --------------------------
   -- Create_Property_List --
   --------------------------

   procedure Create_Property_List
     (Tree_View : Gtk.Tree_View.Gtk_Tree_View)
   is
      Model       : Gtk.Tree_Store.Gtk_Tree_Store;
      Text_Render : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Text_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Num         : Glib.Gint;
      pragma Unreferenced (Num);
   begin
      Gtk.Tree_Store.Gtk_New
        (Model,
         (0     => Glib.GType_String,
          1     => Glib.GType_String));

      Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
      Gtk.Tree_View_Column.Gtk_New (Text_Column);
      Num := Tree_View.Append_Column (Text_Column);
      Text_Column.Pack_Start (Text_Render, True);
      Text_Column.Set_Sizing
        (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
      Text_Column.Add_Attribute (Text_Render, "text", 0);

      Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
      Gtk.Tree_View_Column.Gtk_New (Text_Column);
      Num := Tree_View.Append_Column (Text_Column);
      Text_Column.Pack_Start (Text_Render, True);
      Text_Column.Set_Sizing
        (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
      Text_Column.Add_Attribute (Text_Render, "text", 1);

      Tree_View.Set_Model
        (Gtk.Tree_Model.Gtk_Tree_Model (Model.To_Interface));

   end Create_Property_List;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler (W : access Gtk.Widget.Gtk_Widget_Record'Class) is
      pragma Unreferenced (W);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   -------------
   -- On_Idle --
   -------------

--     overriding procedure On_Idle
--       (State : in out Carthage_UI)
--     is
--        use Carthage.Calendar;
--        New_Date : constant Time := Clock;
--        pragma Unreferenced (New_Date);
--     begin
--        Carthage.Updates.Update;
--  --        if New_Date /= State.Date then
--  --           State.Date := New_Date;
--  --           State.Date_Label.Set_Label
--  --             (Carthage.Calendar.Image (State.Date));
--  --        end if;
--
--        for I in 1 .. State.Models.Count loop
--           State.Models.Model (I).Queue_Render;
--        end loop;
--
--     end On_Idle;

   ----------------------------
   -- On_Step_Button_Clicked --
   ----------------------------

   procedure On_Step_Button_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is null;
      --  Name      : constant String := Button.Get_Name;
      --  New_Speed : constant Natural :=
      --                Character'Pos (Name (Name'Last)) - 48;
   --  begin
   --     Carthage.Updates.Set_Time_Acceleration
   --       (Duration (New_Speed * 84_600 / 3));
   --  end On_Step_Button_Clicked;

   --------------------
   -- On_Switch_Page --
   --------------------

--     procedure On_Switch_Page
--       (Self     : access Glib.Object.GObject_Record'Class;
--        Page     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
--        Page_Num : Glib.Guint)
--     is
--        pragma Unreferenced (Page);
--        UI : Carthage_UI renames Carthage_UI (Self.all);
--        Model : constant Lui.Models.Object_Model :=
--                  UI.Models.Model (Natural (Page_Num) + 1);
--     begin
--        Lui.Gtk_UI.On_Model_Activation (Model);
--        UI.Show_Properties (Model);
--     end On_Switch_Page;

   --------------------
   -- Select_Feature --
   --------------------

   overriding procedure Select_Feature
     (To      : in out Carthage_UI;
      Feature : Lui.Lui_UI_Feature;
      Element : not null access Lui.Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use Lui;
      use type Gtk.Widget.Gtk_Widget;
   begin
      case Feature is
         when UI_Gadget =>
            null;
         when UI_Model =>
            declare
               Model : constant Lui.Models.Object_Model :=
                         Lui.Models.Object_Model (Element);
            begin
               if To.Active_Area /= null then
                  To.Model_Area.Remove (To.Active_Area);
               end if;
               To.Model_Area.Add (Top);
               To.Active_Area := Gtk.Widget.Gtk_Widget (Top);
               To.Show_Properties (Model);
            end;
         when UI_Table =>
            null;
            --              To.Info_Boxes.Add (Top);
      end case;
   end Select_Feature;

   ---------------------
   -- Show_Properties --
   ---------------------

   procedure Show_Properties
     (UI      : Carthage_UI'Class;
      Model   : Lui.Models.Object_Model)
   is null;
--        Count : constant Natural :=
--                  Model.Property_Count;
--        Store : constant Gtk.Tree_Store.Gtk_Tree_Store :=
--                  Gtk.Tree_Store.Gtk_Tree_Store
--                    (Gtk.Tree_Model."-" (UI.Property_List.Get_Model));
--     begin
--        Store.Clear;
--        for I in 1 .. Count loop
--           declare
--              Result : Gtk.Tree_Model.Gtk_Tree_Iter;
--              Name   : constant String :=
--                         Model.Property_Name (I);
--              Value  : constant String :=
--                         Model.Property_Value (I);
--           begin
--              Store.Append (Result, Gtk.Tree_Model.Null_Iter);
--              Store.Set (Result, 0, Name);
--              Store.Set (Result, 1, Value);
--           end;
--        end loop;
--     end Show_Properties;

   -----------
   -- Start --
   -----------

   procedure Start (House : Carthage.Handles.Houses.House_Handle) is
      Builder : Gtk.Builder.Gtk_Builder;
      UI_Path : constant String :=
                  Carthage.Paths.Config_File
                    ("ui/carthage.ui");
   begin

      Lui.Rendering.Set_Image_Path
        (Carthage.Paths.Config_Path & "/images");

      Gtk.Main.Init;

      Gtk.Builder.Gtk_New (Builder);

      Ada.Text_IO.Put_Line ("Loading: " & UI_Path);

      declare
         use type Glib.Guint;
         Error : aliased Glib.Error.GError;
         Result : constant Glib.Guint :=
                    Builder.Add_From_File
                      (Filename => UI_Path,
                       Error    => Error'Access);
      begin
         if Result = 0 then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Error opening GUI definition: " & UI_Path
               & ": " & Glib.Error.Get_Message (Error));
            return;
         end if;
      end;

      Ada.Text_IO.Put_Line ("done");

      declare
         Main_Window : constant Gtk.Window.Gtk_Window :=
                         Gtk.Window.Gtk_Window
                           (Builder.Get_Object ("Carthage_Main_Window"));
      begin
         Main_Window.On_Destroy (Destroy_Handler'Access);
         Main_Window.Set_Hide_Titlebar_When_Maximized (True);
         Main_Window.Show_All;
         Main_Window.Maximize;
      end;

      declare
         Model_Area : constant Gtk.Container.Gtk_Container :=
                        Gtk.Container.Gtk_Container
                          (Builder.Get_Object ("Carthage_Main_Window"));
         Models   : Lui.Models.Active_Model_List;
         UI         : constant Carthage_UI_Access :=
                        new Carthage_UI'
                          (Glib.Object.GObject_Record with
                           Models         => Models,
                           Model_Area     => Model_Area,
                           Active_Area    => null);
         Top        : constant Carthage.UI.Models.Top.Top_Model :=
                        Carthage.UI.Models.Top.Create_Top_Model (House);
      begin

         UI.Initialize;
         Lui.Gtk_UI.Start
           (Main => UI,
            Top  => Lui.Models.Object_Model (Top));

         Top.Show_Galaxy;

      end;

      Gtk.Main.Main;

   end Start;

end Carthage.UI.Gtk_UI;
