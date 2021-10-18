with Ada.Strings.Fixed;

package body Carthage.UI.Models.Stacks is

   ---------------
   -- Add_Stack --
   ---------------

   procedure Add_Stack
     (List          : in out Rendered_Stack_List'Class;
      Stack         : Carthage.Handles.Stacks.Stack_Handle;
      Left, Top     : Integer;
      Width, Height : Natural)
   is
   begin
      List.Append
        (Rendered_Stack_Record'
           (Stack  => Stack,
            Left   => Left,
            Top    => Top,
            Width  => Width,
            Height => Height));
   end Add_Stack;

   -----------
   -- Clear --
   -----------

   procedure Clear (List : in out Rendered_Stack_List'Class) is
   begin
      Rendered_Stack_Lists.List (List).Clear;
   end Clear;

   ----------------
   -- Find_Stack --
   ----------------

   function Find_Stack
     (List : Rendered_Stack_List'Class;
      X, Y : Integer)
      return Carthage.Handles.Stacks.Stack_Handle
   is
   begin
      for Rec of List loop
         if X in Rec.Left .. Rec.Left + Rec.Width
           and then Y in Rec.Top .. Rec.Top + Rec.Height
         then
            return Rec.Stack;
         end if;
      end loop;
      return Carthage.Handles.Stacks.Empty_Handle;
   end Find_Stack;

   ------------
   -- Render --
   ------------

   procedure Render
     (List     : Rendered_Stack_List'Class;
      Model    : Lui.Models.Root_Object_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      pragma Unreferenced (Model);
   begin
      for Rec of List loop
         declare
            House      : constant Carthage.Handles.Houses.House_Handle :=
                           Rec.Stack.Owner;
            Background : Carthage.Colors.Color_Type :=
                           House.Color;
            Resource   : constant String :=
                           (if not Rec.Stack.Has_Assets then ""
                            else Carthage.Handles.Assets.Get
                              (Rec.Stack.Asset (1))
                            .Unit.Resource_Name);
            Size_Text  : constant String :=
                           Ada.Strings.Fixed.Trim
                             (Rec.Stack.Asset_Count'Image,
                              Ada.Strings.Left);
            Icon_Size  : constant Natural :=
                           Natural'Min (Rec.Width, Rec.Height);
         begin
            Background.Alpha := 0.7;

            Renderer.Set_Color (To_Lui_Color (Background));
            Renderer.Rectangle
              (Rec.Left, Rec.Top, Rec.Width, Rec.Height, True);

            if Rec.Stack.Has_Assets then
               Renderer.Image
                 ((Rec.Left, Rec.Top, Icon_Size, Icon_Size), Resource);
            end if;
            Renderer.Set_Color (Lui.Colors.Black);
            Renderer.Rectangle
              (Rec.Left + Rec.Width - 12, Rec.Top + Rec.Height - 8,
               12, 8, True);
            Renderer.Set_Color (Lui.Colors.White);
            Renderer.Text
              (Rec.Left + Rec.Width - 10, Rec.Top + Rec.Height,
               Size_Text);
         end;
      end loop;
   end Render;

end Carthage.UI.Models.Stacks;
