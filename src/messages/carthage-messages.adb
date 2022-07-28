with WL.String_Maps;

with Carthage.Logging;

package body Carthage.Messages is

   package Message_Bank_Maps is
     new WL.String_Maps (Message_Bank_Reference);

   Message_Bank_Map : Message_Bank_Maps.Map;

   type Null_Message_Bank_Instance is
     new Message_Interface with null record;

   overriding function Tag (This : Null_Message_Bank_Instance) return String
   is ("null-message");

   overriding function Description
     (This : Null_Message_Bank_Instance)
      return String
   is ("message with no information");

   protected body Message_Bank_Type is

      -----------
      -- Close --
      -----------

      procedure Close is
      begin
         Closing := True;
      end Close;

      ---------
      -- Get --
      ---------

      entry Get (Message : out Message_Holders.Holder)
        when not List.Is_Empty or else Closing
      is
      begin
         if List.Is_Empty then
            Message := Message_Holders.Empty_Holder;
         else
            Message := Message_Holders.To_Holder (List.First_Element);
            List.Delete_First;
         end if;
      end Get;

      ----------
      -- Send --
      ----------

      procedure Send (Message : Message_Interface'Class) is
      begin
         List.Append (Message);
      end Send;

   end Message_Bank_Type;

   -----------
   -- Close --
   -----------

   procedure Close
     (This : in out Message_Bank)
   is
   begin
      This.Protected_Bank.Close;
   end Close;

   -------------------------
   -- Create_Message_Bank --
   -------------------------

   function Create_Message_Bank
     (Tag : String)
      return Message_Bank_Reference
   is
   begin
      return Bank : constant Message_Bank_Reference := new Message_Bank
      do
         Bank.Protected_Bank := new Message_Bank_Type;
         Message_Bank_Map.Insert (Tag, Bank);
      end return;
   end Create_Message_Bank;

   ------------------
   -- Next_Message --
   ------------------

   function Next_Message
     (This : Message_Bank)
      return Message_Interface'Class
   is
      Message : Message_Holders.Holder;
   begin
      This.Protected_Bank.Get (Message);
      if Message.Is_Empty then
         return Null_Message;
      else
         return Message.Element;
      end if;
   end Next_Message;

   ------------------
   -- Null_Message --
   ------------------

   function Null_Message return Message_Interface'Class is
   begin
      return Null_Message_Bank_Instance'(null record);
   end Null_Message;

   ----------
   -- Send --
   ----------

   procedure Send
     (This    : in out Message_Bank;
      Message : Message_Interface'Class)
   is
   begin
      This.Protected_Bank.Send (Message);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send (This : Message_Interface'Class) is
      Position : constant Message_Bank_Maps.Cursor :=
                   Message_Bank_Map.Find (This.Tag);
   begin
      if Message_Bank_Maps.Has_Element (Position) then
         Message_Bank_Maps.Element (Position).Send (This);
      else
         Carthage.Logging.Log ("no bank with tag: " & This.Tag);
      end if;
   end Send;

end Carthage.Messages;
