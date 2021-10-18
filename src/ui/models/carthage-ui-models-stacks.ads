private with Ada.Containers.Doubly_Linked_Lists;

with Lui.Rendering;

with Carthage.Handles.Assets;
with Carthage.Handles.Stacks;

package Carthage.UI.Models.Stacks is

   type Rendered_Stack_List is tagged private;

   function Is_Empty (List : Rendered_Stack_List'Class) return Boolean;

   procedure Clear (List : in out Rendered_Stack_List'Class);

   procedure Add_Stack
     (List          : in out Rendered_Stack_List'Class;
      Stack         : Carthage.Handles.Stacks.Stack_Handle;
      Left, Top     : Integer;
      Width, Height : Natural);

   procedure Render
     (List     : Rendered_Stack_List'Class;
      Model    : Lui.Models.Root_Object_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   function Find_Stack
     (List : Rendered_Stack_List'Class;
      X, Y : Integer)
      return Carthage.Handles.Stacks.Stack_Handle;

   function Asset_Resource
     (Asset : Carthage.Handles.Assets.Asset_Handle)
      return String;

private

   type Rendered_Stack_Record is
      record
         Stack         : Carthage.Handles.Stacks.Stack_Handle;
         Left, Top     : Integer;
         Width, Height : Natural;
      end record;

   package Rendered_Stack_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Rendered_Stack_Record);

   type Rendered_Stack_List is new Rendered_Stack_Lists.List with null record;

   function Is_Empty (List : Rendered_Stack_List'Class) return Boolean
   is (Rendered_Stack_Lists.List (List).Is_Empty);

   function Asset_Resource
     (Asset : Carthage.Handles.Assets.Asset_Handle)
      return String
   is (Asset.Unit.Resource_Name);

end Carthage.UI.Models.Stacks;
