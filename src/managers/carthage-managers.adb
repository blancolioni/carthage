with Ada.Containers.Doubly_Linked_Lists;

with Carthage.Logging;

package body Carthage.Managers is

   package Manager_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Reference);

   Manager_List : Manager_Lists.List;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This  : in out Instance'Class;
      House : Carthage.Handles.Houses.House_Handle)
   is
   begin
      This.House := House;
   end Initialize;

   ---------
   -- Log --
   ---------

   procedure Log (This : Instance'Class; Message : String) is
   begin
      Carthage.Logging.Log (This.Log_Name, Message);
   end Log;

   --------------
   -- Log_Name --
   --------------

   function Log_Name (This : Instance) return String is
   begin
      return Dispatch (This).Name;
   end Log_Name;

   ------------------
   -- Save_Manager --
   ------------------

   procedure Save_Manager (Manager : not null access Instance'Class) is
   begin
      Manager.Log ("created");
      Manager_List.Append (Reference (Manager));
   end Save_Manager;

   -------------------
   -- Stop_Managers --
   -------------------

   procedure Stop_Managers is
   begin
      for Manager of reverse Manager_List loop
         Manager.Stop;
      end loop;
      Manager_List.Clear;
   end Stop_Managers;

end Carthage.Managers;
