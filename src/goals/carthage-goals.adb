package body Carthage.Goals is

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First
     (Queue : in out Goal_Queue)
   is
      use Goal_Maps;
      Map_Position : Cursor := Queue.Map.First;
   begin
      pragma Assert (Has_Element (Map_Position));

      declare
         List : Goal_Lists.List renames
                  Queue.Map (Map_Position);
      begin
         List.Delete_First;
         if List.Is_Empty then
            Queue.Map.Delete (Map_Position);
         end if;
      end;
   end Delete_First;

   ----------------
   -- First_Goal --
   ----------------

   function First_Goal
     (Queue : Goal_Queue)
      return Goal_Record'Class
   is
   begin
      pragma Assert (not Queue.Map.Is_Empty, "First_Goal: empty queue");
      return Queue.Map.First_Element.First_Element;
   end First_Goal;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Queue : in out Goal_Queue;
      Goal  : Goal_Record'Class)
   is
      use Goal_Maps;
      Map_Position : constant Cursor := Queue.Map.Find (Goal.Priority);
   begin
      if Has_Element (Map_Position) then
         Queue.Map (Map_Position).Append (Goal);
      else
         declare
            List : Goal_Lists.List;
         begin
            List.Append (Goal);
            Queue.Map.Insert (Goal.Priority, List);
         end;
      end if;
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Queue   : Goal_Queue;
      Process : not null access
        procedure (Goal     : Goal_Record'Class))
   is
   begin
      for List of Queue.Map loop
         for Goal of List loop
            Process (Goal);
         end loop;
      end loop;
   end Iterate;

   ------------
   -- Update --
   ------------

   procedure Update
     (Queue   : in out Goal_Queue;
      Process : not null access
        procedure (Goal     : in out Goal_Record'Class;
                   Complete : out Boolean))
   is
      New_Queue : Goal_Queue;
   begin
      for List of Queue.Map loop
         for Goal of List loop
            declare
               Complete : Boolean;
            begin
               Process (Goal, Complete);
               if not Complete then
                  New_Queue.Insert (Goal);
               end if;
            end;
         end loop;
      end loop;
      Queue := New_Queue;
   end Update;

end Carthage.Goals;
