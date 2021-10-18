private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Ordered_Maps;

package Carthage.Goals is

   type Goal_Priority is range 1 .. 100;

   Highest_Priority : constant Goal_Priority := Goal_Priority'First;
   Middle_Priority  : constant Goal_Priority := Goal_Priority'Last / 2;
   Lowest_Priority  : constant Goal_Priority := Goal_Priority'Last;

   type Goal_Record (Priority : Goal_Priority) is
     abstract tagged private;

   function Show (Goal : Goal_Record) return String is abstract;

   type Goal_Queue is tagged private;

   function Is_Empty (Queue : Goal_Queue) return Boolean;

   procedure Insert
     (Queue : in out Goal_Queue;
      Goal : Goal_Record'Class);

   function First_Goal
     (Queue : Goal_Queue)
      return Goal_Record'Class;

   procedure Delete_First
     (Queue : in out Goal_Queue);

   procedure Iterate
     (Queue : Goal_Queue;
      Process : not null access
        procedure (Goal : Goal_Record'Class));

   procedure Update
     (Queue   : in out Goal_Queue;
      Process : not null access
        procedure (Goal     : in out Goal_Record'Class;
                   Complete : out Boolean));

private

   type Goal_Record (Priority : Goal_Priority) is
     abstract tagged null record;

   package Goal_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Goal_Record'Class);

   package Goal_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Goal_Priority,
        Element_Type => Goal_Lists.List,
        "<"          => "<",
        "="          => Goal_Lists."=");

   type Goal_Queue is tagged
      record
         Map : Goal_Maps.Map;
      end record
   with Invariant => (for all List of Goal_Queue.Map => not List.Is_Empty);

   function Is_Empty (Queue : Goal_Queue) return Boolean
   is (Queue.Map.Is_Empty);

end Carthage.Goals;
