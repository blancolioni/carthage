with Carthage.Handles.Houses;

package Carthage.Managers is

   type Instance is abstract tagged private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   function Name (This : Instance) return String is abstract;
   procedure Stop (This : in out Instance) is null;

   function Log_Name (This : Instance) return String;

   function House
     (This : Instance)
      return Carthage.Handles.Houses.House_Handle;

   procedure Log
     (This : Instance'Class;
      Message : String);

   procedure Initialize
     (This  : in out Instance'Class;
      House : Carthage.Handles.Houses.House_Handle);

   procedure Save_Manager
     (Manager : not null access Instance'Class);

   procedure Stop_Managers;

private

   subtype Dispatch is Instance'Class;

   type Instance is abstract tagged
      record
         House : Carthage.Handles.Houses.House_Handle;
      end record;

   function House
     (This : Instance)
      return Carthage.Handles.Houses.House_Handle
   is (This.House);

end Carthage.Managers;
