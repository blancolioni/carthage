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

   -------------------
   -- Finish_Update --
   -------------------

   procedure Finish_Update (House : House_Handle) is
   begin
      House.Log_Status;
      House.Save_History;
   end Finish_Update;

end Carthage.Handles.Houses.Updates;
