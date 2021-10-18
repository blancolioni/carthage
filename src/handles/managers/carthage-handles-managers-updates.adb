package body Carthage.Handles.Managers.Updates is

   --------------------
   -- Start_Managers --
   --------------------

   procedure Start_Managers is
   begin
      For_All_Active_Managers (Start'Access);
   end Start_Managers;

end Carthage.Handles.Managers.Updates;
