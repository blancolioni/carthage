--  with WL.String_Maps;

--  with Carthage.Handles.Planets.Configure;

package body Carthage.Handles.Galaxy.Configure is

   ---------------------
   -- Configure_Gates --
   ---------------------

   procedure Configure_Gates
     (Gate_Config : Tropos.Configuration)
   is

      function Planet
        (Config : Tropos.Configuration)
         return Carthage.Handles.Planets.Planet_Handle
      is (if Carthage.Handles.Planets.Exists (Config.Config_Name)
          then Carthage.Handles.Planets.Get (Config.Config_Name)
          else raise Constraint_Error with
            "gates: " & Config.Config_Name & ": no such planet");

      procedure Check (Planet : Carthage.Handles.Planets.Planet_Handle);

      -----------
      -- Check --
      -----------

      procedure Check (Planet : Carthage.Handles.Planets.Planet_Handle) is
         use type Planet_Graph.Count_Type;

         procedure Check_Reverse_Connection
           (To   : Carthage.Handles.Planets.Planet_Handle;
            Cost : Non_Negative_Real);

         ------------------------------
         -- Check_Reverse_Connection --
         ------------------------------

         procedure Check_Reverse_Connection
           (To   : Carthage.Handles.Planets.Planet_Handle;
            Cost : Non_Negative_Real)
         is
            pragma Unreferenced (Cost);
         begin
            if not Graph.Connected (To.Reference, Planet.Reference) then
               Planet.Log
                 ("warning: connection from "
                  & Planet.Tag & " to " & To.Tag
                  & " is one-way");
            end if;
         end Check_Reverse_Connection;

      begin
         if Graph.Edge_Count (Planet.Reference) = 0 then
            Planet.Log ("warning: no connections");
         else
            Graph.Iterate_Edges
              (Planet, Check_Reverse_Connection'Access);
         end if;
      end Check;

   begin

      declare
         procedure Add_Planet
           (Planet : Carthage.Handles.Planets.Planet_Handle);

         ----------------
         -- Add_Planet --
         ----------------

         procedure Add_Planet
           (Planet : Carthage.Handles.Planets.Planet_Handle)
         is
         begin
            Graph.Append (Planet);
         end Add_Planet;

      begin
         Carthage.Handles.Planets.For_All_Planets (Add_Planet'Access);
      end;

      for From_Config of Gate_Config loop
         declare
            use Carthage.Handles.Planets;
            From : constant Planet_Handle := Planet (From_Config);
         begin
            for To_Config of From_Config loop
               declare
                  To : constant Planet_Handle := Planet (To_Config);
               begin
                  Graph.Connect (From.Reference, To.Reference);
               end;
            end loop;
         end;
      end loop;

      Carthage.Handles.Planets.For_All_Planets (Check'Access);

   end Configure_Gates;

   -------------------------
   -- Configure_Positions --
   -------------------------

   --  procedure Configure_Positions
   --    (Position_Config : Tropos.Configuration)
   --  is
   --     package Has_Position_Maps is
   --       new WL.String_Maps (Boolean);
   --
   --     Have_Position : Has_Position_Maps.Map;
   --     Have_Size     : Boolean := False;
   --     Width, Height : Float;
   --  begin
   --     for Config of Position_Config loop
   --        declare
   --           X : constant Float := Config.Get (1);
   --           Y : constant Float := Config.Get (2);
   --        begin
   --           if not Have_Size then
   --              Width := X;
   --              Height := Y;
   --              Have_Size := True;
   --           else
   --              if Have_Position.Contains (Config.Config_Name) then
   --                 Carthage.Handles.Planets.Get (Config.Config_Name).Log
   --                    ("warning: repeated position");
   --              else
   --                 Have_Position.Insert (Config.Config_Name, True);
   --                 Carthage.Handles.Planets.Configure.Configure_Position
   --                   (Config.Config_Name,
   --                    Carthage.Handles.Planets.Coordinate (X / Width),
   --                    Carthage.Handles.Planets.Coordinate (Y / Height));
   --              end if;
   --           end if;
   --        end;
   --     end loop;
   --
   --     declare
   --        procedure Check (Planet : Carthage.Handles.Planets.Planet_Handle);
   --
   --        -----------
   --        -- Check --
   --        -----------
   --
   --     procedure Check (Planet : Carthage.Handles.Planets.Planet_Handle) is
   --        begin
   --           if not Have_Position.Contains (Planet.Tag) then
   --              Planet.Log ("warning: no position information");
   --           end if;
   --        end Check;
   --
   --     begin
   --        Carthage.Handles.Planets.For_All_Planets (Check'Access);
   --     end;
   --
   --  end Configure_Positions;

   -----------------
   -- Import_Gate --
   -----------------

   procedure Import_Gate
     (From, To : Planet_Reference)
   is
      use Carthage.Handles.Planets;
   begin
      if Graph.Is_Empty then
         declare
            procedure Add_Planet (Planet : Planet_Handle);

            ----------------
            -- Add_Planet --
            ----------------

            procedure Add_Planet (Planet : Planet_Handle) is
            begin
               Graph.Append (Planet);
            end Add_Planet;

         begin
            Carthage.Handles.Planets.For_All_Planets (Add_Planet'Access);
         end;
      end if;

      Graph.Connect (From, To, 1.0);
      Graph.Connect (To, From, 1.0);

   end Import_Gate;

end Carthage.Handles.Galaxy.Configure;
