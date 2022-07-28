private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Carthage.Messages is

   type Message_Interface is interface;

   function Tag (This : Message_Interface) return String is abstract;
   function Description (This : Message_Interface) return String is abstract;

   procedure Send (This : Message_Interface'Class);

   function Null_Message return Message_Interface'Class;

   type Message_Bank is tagged private;
   type Message_Bank_Reference is access all Message_Bank'Class;

   procedure Send
     (This    : in out Message_Bank;
      Message : Message_Interface'Class);

   function Next_Message
     (This : Message_Bank)
      return Message_Interface'Class;

   procedure Close
     (This : in out Message_Bank);

   function Create_Message_Bank
     (Tag : String)
      return Message_Bank_Reference;

private

   package Message_Holders is
     new Ada.Containers.Indefinite_Holders (Message_Interface'Class);

   package Message_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Message_Interface'Class);

   protected type Message_Bank_Type is
      procedure Send (Message : Message_Interface'Class);
      entry Get (Message : out Message_Holders.Holder);
      procedure Close;
   private
      List    : Message_Lists.List;
      Closing : Boolean := False;
   end Message_Bank_Type;

   type Protected_Bank_Access is access all Message_Bank_Type;

   type Message_Bank is tagged
      record
         Protected_Bank : Protected_Bank_Access;
      end record;

end Carthage.Messages;
