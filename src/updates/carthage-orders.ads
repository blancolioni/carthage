package Carthage.Orders is

   type Order_Interface is interface;

   function Description (This : Order_Interface) return String is abstract;
   procedure Execute (This : Order_Interface) is abstract;

end Carthage.Orders;
