with Ada.Containers.Vectors;

package body Carthage.Handles.Technology.Configure is

   package Tec_Vectors is
     new Ada.Containers.Vectors (Natural, Technology_Reference);

   Tec_Vector : Tec_Vectors.Vector;

   --------------------------
   -- Configure_Technology --
   --------------------------

   procedure Configure_Technology
     (Config : Tropos.Configuration)
   is
      Index : constant Natural := Config.Get ("index");
      Reference : constant Technology_Reference :=
                    Create (Tag       => Config.Config_Name,
                            Like      => Config.Get ("like"),
                            Cost      => Research_Points
                              (Natural'(Config.Get ("cost"))));
   begin
      while Tec_Vector.Last_Index < Index loop
         Tec_Vector.Append (Null_Technology_Reference);
      end loop;
      Tec_Vector (Index) := Reference;
   end Configure_Technology;

   --------------------
   -- Configure_Tree --
   --------------------

   procedure Configure_Tree
     (Config : Tropos.Configuration)
   is
      Tec : constant Technology_Handle := Get (Config.Config_Name);
   begin
      for Enabled_Config of Config loop
         if Enabled_Config.Config_Name = "enabled-by" then
            declare
               Enabler : constant String := Enabled_Config.Value;
            begin
               pragma Assert (Exists (Enabler),
                              "no such tec enabler: " & Enabler);
               Tec.Set_Enabled_By (Get (Enabler));
            end;
         end if;
      end loop;
   end Configure_Tree;

end Carthage.Handles.Technology.Configure;
