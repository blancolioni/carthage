with Carthage.Money;

package Carthage.Handles.Houses.Updates is

   procedure Execute_Payment
     (From, To : House_Handle;
      Amount   : Carthage.Money.Money_Type);

   procedure Finish_Update (House : House_Handle);

end Carthage.Handles.Houses.Updates;
