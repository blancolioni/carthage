with WL.Localisation;

with Carthage.Logging;

package body Carthage.Handles is

   -----------
   -- Clear --
   -----------

   procedure Clear (Set : in out Set_Of_Houses) is
   begin
      Set := (others => False);
   end Clear;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Set : in out Set_Of_Houses; House : House_Reference) is
   begin
      Set (House) := False;
   end Exclude;

   -------------
   -- Include --
   -------------

   procedure Include (Set : in out Set_Of_Houses; House : House_Reference) is
   begin
      Set (House) := True;
   end Include;

   -----------------
   -- Information --
   -----------------

   procedure Information
     (Handle  : Root_Carthage_Handle'Class;
      Message : String)
   is
   begin
      Carthage.Logging.Log
        (Level   => Carthage.Logging.Log_Level'Last,
         Message =>
           Handle.Log_Identifier
         & ": "
         & Message);
   end Information;

   ----------------
   -- Local_Text --
   ----------------

   function Local_Text (Localised : Localised_Interface'Class) return String is
   begin
      return WL.Localisation.Local_Text (Localised.Localisation_Tag);
   end Local_Text;

   ---------
   -- Log --
   ---------

   procedure Log (Handle  : Root_Carthage_Handle'Class;
                  Message : String)
   is
   begin
      Carthage.Logging.Log
        (Handle.Log_Identifier
         & ": "
         & Message);
   end Log;

   ---------------------
   -- Next_Identifier --
   ---------------------

   function Next_Identifier return Object_Identifier is

      function Inc (Ch : in out Character) return Boolean
        with Pre => Ch in 'A' .. 'Z' | '0' .. '9';

      ---------
      -- Inc --
      ---------

      function Inc (Ch : in out Character) return Boolean is
      begin
         if Ch = 'Z' then
            Ch := 'A';
            return True;
         elsif Ch = '9' then
            Ch := '0';
            return True;
         else
            Ch := Character'Succ (Ch);
            return False;
         end if;
      end Inc;

   begin
      return Id : Object_Identifier := Current_Identifier do
         for Ch of reverse Id loop
            exit when not Inc (Ch);
         end loop;
         Current_Identifier := Id;
      end return;
   end Next_Identifier;

end Carthage.Handles;
