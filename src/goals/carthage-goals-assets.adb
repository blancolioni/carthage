with Carthage.Handles.Assets;

package body Carthage.Goals.Assets is

   --------------------------
   -- Add_Asset_To_Manager --
   --------------------------

   function Add_Asset_To_Manager
     (Asset : Carthage.Handles.Asset_Reference)
      return Goal_Record'Class
   is
   begin
      return Add_Asset_Goal_Record'
        (Priority => Middle_Priority,
         Asset    => Asset);
   end Add_Asset_To_Manager;

   ----------
   -- Show --
   ----------

   overriding function Show (Goal : Add_Asset_Goal_Record) return String is
   begin
      return "add asset: "
        & Carthage.Handles.Assets.Get (Goal.Asset).Log_Identifier;
   end Show;

end Carthage.Goals.Assets;
