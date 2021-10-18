package Carthage.Named is

   type Named_Interface is limited interface;

   function Name (Item : Named_Interface) return String is abstract;

end Carthage.Named;
