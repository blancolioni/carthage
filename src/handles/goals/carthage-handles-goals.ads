with Ada.Streams;

with Carthage.Goals;

package Carthage.Handles.Goals is

   Goal_Queue_Version : constant Object_Version := (0, 1, 0);

   type Goal_Queue_Handle is
     new Root_Carthage_Handle
   with private;

   function Reference (Handle : Goal_Queue_Handle) return Goal_Queue_Reference;
   function Get (Reference : Goal_Queue_Reference) return Goal_Queue_Handle;
   function Empty_Handle return Goal_Queue_Handle;

   function Is_Empty (Queue : Goal_Queue_Handle) return Boolean;

   procedure Insert
     (Queue : Goal_Queue_Handle;
      Goal  : Carthage.Goals.Goal_Record'Class);

   function First_Goal
     (Queue : Goal_Queue_Handle)
      return Carthage.Goals.Goal_Record'Class;

   procedure Delete_First
     (Queue : Goal_Queue_Handle);

   procedure Iterate
     (Queue   : Goal_Queue_Handle;
      Process : not null access
        procedure (Goal : Carthage.Goals.Goal_Record'Class));

   procedure Update
     (Queue   : Goal_Queue_Handle;
      Process : not null access
        procedure (Goal     : in out Carthage.Goals.Goal_Record'Class;
                   Complete : out Boolean));

   function Create_Goal_Queue
     (Name : String)
     return Goal_Queue_Reference;

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

private

   type Goal_Queue_Handle is
     new Root_Carthage_Handle with
      record
         Reference : Goal_Queue_Reference := 0;
      end record;

   overriding function Short_Name
     (Handle : Goal_Queue_Handle)
      return String;

   function Reference (Handle : Goal_Queue_Handle) return Goal_Queue_Reference
   is (Handle.Reference);

   function Get (Reference : Goal_Queue_Reference) return Goal_Queue_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Goal_Queue_Handle
   is (False, 0);

end Carthage.Handles.Goals;
