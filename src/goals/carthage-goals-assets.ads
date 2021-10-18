with Carthage.Handles;

package Carthage.Goals.Assets is

   function Is_Asset_Goal (Goal : Goal_Record'Class) return Boolean;

   function Get_Asset
     (Goal : Goal_Record'Class)
      return Carthage.Handles.Asset_Reference
     with Pre => Is_Asset_Goal (Goal);

   function Add_Asset_To_Manager
     (Asset : Carthage.Handles.Asset_Reference)
      return Goal_Record'Class;

   function Is_Add_Asset_Goal
     (Goal : Goal_Record'Class)
      return Boolean
     with Post => not Is_Add_Asset_Goal'Result or else Is_Asset_Goal (Goal);

private

   type Asset_Goal_Record is abstract new Goal_Record with
      record
         Asset : Carthage.Handles.Asset_Reference;
      end record;

   function Is_Asset_Goal (Goal : Goal_Record'Class) return Boolean
   is (Goal in Asset_Goal_Record'Class);

   function Get_Asset
     (Goal : Goal_Record'Class)
      return Carthage.Handles.Asset_Reference
   is (Asset_Goal_Record'Class (Goal).Asset);

   type Add_Asset_Goal_Record is
     new Asset_Goal_Record with null record;

   overriding function Show (Goal : Add_Asset_Goal_Record) return String;

   function Is_Add_Asset_Goal
     (Goal : Goal_Record'Class)
      return Boolean
   is (Goal in Add_Asset_Goal_Record'Class);

end Carthage.Goals.Assets;
