with Carthage.Calendar;

package Carthage.Updates is

   procedure Before_First_Update;

   procedure Set_Time_Acceleration (Factor : Duration);
   --  for each unit of real time, advance Factor units of game time

   procedure Update;

   type Update_Interface is interface;

   procedure Activate
     (Item : Update_Interface)
   is abstract;

   procedure Queue
     (Item       : Update_Interface'Class;
      Next_Event : Carthage.Calendar.Time);

   procedure Queue
     (Item        : Update_Interface'Class;
      Event_Delay : Duration);

end Carthage.Updates;
