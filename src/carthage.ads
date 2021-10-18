package Carthage with Pure is

   Planet_Width  : constant := 44;
   Planet_Height : constant := 32;

   subtype Tile_X_Count is Natural range 0 .. Planet_Width;
   subtype Tile_Y_Count is Natural range 0 .. Planet_Height;

   subtype Tile_X is Tile_X_Count range 1 .. Planet_Width;
   subtype Tile_Y is Tile_Y_Count range 1 .. Planet_Height;

   type Tile_Position is
      record
         X : Tile_X;
         Y : Tile_Y;
      end record;

   type Array_Of_Positions is array (Positive range <>) of Tile_Position;

   function Tile_Position_Index
     (Position : Tile_Position)
      return Positive
   is (Natural (Position.Y - 1) * Planet_Width + Positive (Position.X));

   type Health_Type is range 0 .. 100;
   type Loyalty_Type is range 0 .. 100;

   type Research_Points is new Natural;

   type Real is new Long_Float range Long_Float'First .. Long_Float'Last;

   subtype Unit_Real is Real range 0.0 .. 1.0;
   subtype Signed_Unit_Real is Real range -1.0 .. 1.0;

   subtype Non_Negative_Real is Real range 0.0 .. Real'Last;

   subtype Real_Time is Real;

   type Carthage_Duration is new Real;

   function Clamp (X : Real;
                   Lo, Hi : Real)
                   return Real
   is (Real'Max (Lo, Real'Min (Hi, X)));

   function Unit_Clamp (X : Real) return Unit_Real
   is (Clamp (X, 0.0, 1.0));

   function Signed_Unit_Clamp (X : Real) return Signed_Unit_Real
   is (Clamp (X, -1.0, 1.0));

end Carthage;
