package body Carthage.Owners is

   ---------------
   -- Has_Owner --
   ---------------

   function Has_Owner (Item : Owned_Interface'Class) return Boolean is
   begin
      return Item.Owner /= null;
   end Has_Owner;

end Carthage.Owners;
