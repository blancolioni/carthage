package body Carthage.Handles.Houses.Updates is

   ---------------------
   -- Execute_Payment --
   ---------------------

   procedure Execute_Payment
     (From, To : House_Handle;
      Amount   : Carthage.Money.Money_Type)
   is
   begin
      From.Spend (Amount);
      To.Earn (Amount);
   end Execute_Payment;

end Carthage.Handles.Houses.Updates;
