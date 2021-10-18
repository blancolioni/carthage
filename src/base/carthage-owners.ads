with Carthage.Named;

package Carthage.Owners is

   type Owner_Interface is
   limited interface and Carthage.Named.Named_Interface;

   type Owner_Type is access all Owner_Interface'Class;

   type Owned_Interface is limited interface;

   function Owner (Item : Owned_Interface)
                   return access constant Owner_Interface'Class
                   is abstract;

   procedure Set_Owner
     (Item : in out Owned_Interface;
      New_Owner : not null access constant Owner_Interface'Class)
   is abstract;

   function Has_Owner (Item : Owned_Interface'Class) return Boolean;

end Carthage.Owners;
