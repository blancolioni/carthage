package body Carthage.Handles.Resources.Configure is

   ------------------------
   -- Configure_Resource --
   ------------------------

   procedure Configure_Resource
     (Config : Tropos.Configuration)
   is
   begin
      Create (Config.Config_Name,
              Carthage.Money.To_Price
                 (Real (Float'(Config.Get ("price")))));
   end Configure_Resource;

end Carthage.Handles.Resources.Configure;
