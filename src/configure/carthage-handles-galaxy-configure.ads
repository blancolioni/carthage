with Tropos;

package Carthage.Handles.Galaxy.Configure is

   procedure Configure_Gates
     (Gate_Config : Tropos.Configuration);

   --  procedure Configure_Positions
   --  (Position_Config : Tropos.Configuration);

   procedure Import_Gate
     (From, To : Planet_Reference);

end Carthage.Handles.Galaxy.Configure;
