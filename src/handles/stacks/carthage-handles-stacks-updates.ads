package Carthage.Handles.Stacks.Updates is

   procedure Start_Update
     (Stack   : Stack_Handle;
      Manager : not null access Stack_Manager_Interface'Class);

   procedure Look (Stack : Stack_Handle);

end Carthage.Handles.Stacks.Updates;
