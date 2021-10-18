--  with Tropos;

package Carthage.Handles.Planets.Configure is

--     procedure Create_Surface_Graph;

   --  procedure Configure_Planet
   --    (Config : Tropos.Configuration);

   --  procedure Configure_Position
   --    (Planet_Id : String;
   --     X, Y      : Coordinate);

   function Import_Planet
     (Id          : String;
      X, Y        : Natural;
      Tile_Set    : Natural;
      Create_Tile : not null access
        function (Planet : Planet_Reference;
                  X      : Tile_X;
                  Y      : Tile_Y)
      return Tile_Reference)
      return Planet_Handle;

   procedure Import_Jump_Gate
     (X1, Y1, X2, Y2 : Natural);

end Carthage.Handles.Planets.Configure;
