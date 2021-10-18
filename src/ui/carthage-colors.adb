package body Carthage.Colors is

   ---------------
   -- Configure --
   ---------------

   function Configure
     (Config : Tropos.Configuration)
      return Color_Type
   is
      function Hex_Digit (Ch : Character) return Natural
      is (if Ch in '0' .. '9'
          then Character'Pos (Ch) - Character'Pos ('0')
          elsif Ch in 'A' .. 'F'
          then Character'Pos (Ch) - Character'Pos ('A') + 10
          elsif Ch in 'a' .. 'f'
          then Character'Pos (Ch) - Character'Pos ('a') + 10
          else 0);

      function Hex_To_Element (Ch : Character) return Color_Element
      is (Color_Element (Real (Hex_Digit (Ch)) / 16.0));

      function Hex_To_Element (S : String) return Color_Element
      is (Color_Element
          (Real
           (Hex_Digit (S (S'First)) * 16
            + Hex_Digit (S (S'First + 1))) / 256.0));

   begin
      if Config.Contains ("color") then
         return Configure (Config.Child ("color"));
      elsif Config.Contains ("Color") then
         return Configure (Config.Child ("Color"));
      elsif Config.Child_Count = 1 then
         declare
            Hex_Name : constant String := Config.Value;
         begin
            if Hex_Name'Length = 3 then
               return (Hex_To_Element (Hex_Name (Hex_Name'First)),
                       Hex_To_Element (Hex_Name (Hex_Name'First + 1)),
                       Hex_To_Element (Hex_Name (Hex_Name'First + 2)),
                       1.0);
            elsif Hex_Name'Length = 4 then
               return (Hex_To_Element (Hex_Name (Hex_Name'First)),
                       Hex_To_Element (Hex_Name (Hex_Name'First + 1)),
                       Hex_To_Element (Hex_Name (Hex_Name'First + 2)),
                       Hex_To_Element (Hex_Name (Hex_Name'First + 3)));
            elsif Hex_Name'Length = 6 then
               declare
                  R : constant String :=
                        Hex_Name (Hex_Name'First .. Hex_Name'First + 1);
                  G : constant String :=
                        Hex_Name (Hex_Name'First + 2 .. Hex_Name'First + 3);
                  B : constant String :=
                        Hex_Name (Hex_Name'First + 4 .. Hex_Name'First + 5);
               begin
                  return (Hex_To_Element (R), Hex_To_Element (G),
                          Hex_To_Element (B), 1.0);
               end;
            elsif Hex_Name'Length = 8 then
               declare
                  R : constant String :=
                        Hex_Name (Hex_Name'First .. Hex_Name'First + 1);
                  G : constant String :=
                        Hex_Name (Hex_Name'First + 2 .. Hex_Name'First + 3);
                  B : constant String :=
                        Hex_Name (Hex_Name'First + 4 .. Hex_Name'First + 5);
               begin
                  return (Hex_To_Element (R), Hex_To_Element (G),
                          Hex_To_Element (B), 1.0);
               end;
            else
               return (1.0, 1.0, 1.0, 1.0);
            end if;
         end;
      elsif Config.Child_Count = 3 then
         declare
            R : constant Float := Config.Get (1);
            G : constant Float := Config.Get (2);
            B : constant Float := Config.Get (3);
         begin
            if R > 1.0 or else G > 1.0 or else B > 1.0 then
               return (Red   => Color_Element (R / 255.0),
                       Green => Color_Element (G / 255.0),
                       Blue  => Color_Element (B / 255.0),
                       Alpha => Color_Element (1.0));
            else
               return (Color_Element (R), Color_Element (G),
                       Color_Element (B), 1.0);
            end if;
         end;
      else
         declare
            R : constant Float := Config.Get ("r", 0.0);
            G : constant Float := Config.Get ("g", 0.0);
            B : constant Float := Config.Get ("b", 0.0);
            A : constant Float := Config.Get ("a", 255.0);
         begin
            return (Red   => Color_Element (R / 255.0),
                    Green => Color_Element (G / 255.0),
                    Blue  => Color_Element (B / 255.0),
                    Alpha => Color_Element (A / 255.0));
         end;
      end if;
   end Configure;

   ------------
   -- Decode --
   ------------

   function Decode (Value : Natural) return Color_Type is
      R : constant Float := Float (Value / 65536);
      G : constant Float := Float (Value / 256 mod 256);
      B : constant Float := Float (Value mod 256);
      A : constant Float := 0.0;
   begin
      return (Red   => Color_Element (R / 255.0),
              Green => Color_Element (G / 255.0),
              Blue  => Color_Element (B / 255.0),
              Alpha => Color_Element (A / 255.0));
   end Decode;

   ------------
   -- Encode --
   ------------

   function Encode (Color : Color_Type) return Natural is
   begin
      return Natural (Color.Red * 255.0) * 65536
        + Natural (Color.Green * 255.0) * 256
        + Natural (Color.Blue * 255.0);
   end Encode;

end Carthage.Colors;
