package body Carthage.Calendar is

   type Time_Element is (Seconds, Minutes, Hours,
                         Days, Months, Years);

   type Element_Converter is
      record
         Denominator : Time;
         Modulus     : Time;
         Base        : Time;
      end record;

   type Time_Converters is array (Time_Element) of Element_Converter;

   Converters : constant Time_Converters :=
                  (Seconds => (1, 60, 0),
                   Minutes => (60, 60, 0),
                   Hours   => (3600, 24, 0),
                   Days    => (24 * 3600, 30, 1),
                   Months  => (30 * 24 * 3600, 12, 1),
                   Years   => (12 * 30 * 24 * 3600, 0, 4551));

   function Convert
     (T       : Time;
      Element : Time_Element)
      return Integer;

   function Date_Image
     (Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number)
      return String;

   function Time_Image
     (Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration := 0.0)
      return String;

   -------------
   -- Advance --
   -------------

   procedure Advance (Seconds : Duration) is
   begin
      Current_Clock := Current_Clock + Seconds;
   end Advance;

   -------------
   -- Convert --
   -------------

   function Convert
     (T       : Time;
      Element : Time_Element)
      return Integer
   is
      X : Time := T;
      Convert : constant Element_Converter := Converters (Element);
   begin
      X := X / Convert.Denominator;
      if Convert.Modulus > 0 then
         X := X mod Convert.Modulus;
      end if;
      X := X + Convert.Base;
      return Integer (X);
   end Convert;

   ----------------
   -- Date_Image --
   ----------------

   function Date_Image
     (Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number)
      return String
   is
      Y : constant String := Integer'Image (Year + 10_000);
      M : constant String := Integer'Image (Month + 100);
      D : constant String := Integer'Image (Day + 100);
   begin
      return Y (3 .. 6) & "-" & M (3 .. 4) & "-" & D (3 .. 4);
   end Date_Image;

   ---------
   -- Day --
   ---------

   function Day (Date : Time) return Day_Number is
   begin
      return Convert (Date, Days);
   end Day;

   -----------------
   -- From_String --
   -----------------

   function From_String
     (Image : String)
      return Time
   is
      pragma Unreferenced (Image);
   begin
      return 0;
   end From_String;

   ----------
   -- Hour --
   ----------

   function Hour (Date : Time) return Hour_Number is
   begin
      return Convert (Date, Hours);
   end Hour;

   -----------
   -- Image --
   -----------

   function Image
     (Date                  : Time;
      Include_Time_Fraction : Boolean := False)
      return String
   is
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      if Include_Time_Fraction then
         return Date_Image (Year, Month, Day) & " " &
           Time_Image (Hour, Minute, Second, Sub_Second);
      else
         return Date_Image (Year, Month, Day);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Elapsed_Time          : Duration;
      Include_Time_Fraction : Boolean := False)
      return String
   is
      pragma Unreferenced (Include_Time_Fraction);
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
   begin
      Split (Time (Elapsed_Time),
             Year, Month, Day, Hour, Minute, Second, Sub_Second);
      return Time_Image (Hour, Minute, Second, Sub_Second);
   end Image;

   ------------
   -- Minute --
   ------------

   function Minute (Date : Time) return Minute_Number is
   begin
      return Convert (Date, Minutes);
   end Minute;

   -----------
   -- Month --
   -----------

   function Month (Date : Time) return Month_Number is
   begin
      return Convert (Date, Months);
   end Month;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Current_Clock := 0;
   end Reset;

   ------------
   -- Second --
   ------------

   function Second (Date : Time) return Second_Number is
   begin
      return Convert (Date, Seconds);
   end Second;

   -------------
   -- Seconds --
   -------------

   function Seconds (Date : Time) return Day_Duration is
   begin
      return Duration (Date mod 86_400);
   end Seconds;

   ----------------
   -- Seconds_Of --
   ----------------

   function Seconds_Of
     (Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number := 0;
      Sub_Second : Second_Duration := 0.0)
      return Day_Duration
   is
   begin
      return Duration (Hour * 3600)
        + Duration (Minute * 60)
        + Duration (Second)
        + Sub_Second;
   end Seconds_Of;

   ---------------
   -- Set_Clock --
   ---------------

   procedure Set_Clock (To : Time) is
   begin
      Current_Clock := To;
   end Set_Clock;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration)
   is
   begin
      Year := Carthage.Calendar.Year (Date);
      Month := Carthage.Calendar.Month (Date);
      Day := Carthage.Calendar.Day (Date);
      Seconds := Carthage.Calendar.Seconds (Date);
   end Split;

   -----------
   -- Split --
   -----------

   procedure Split
     (Seconds    : Day_Duration;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration)
   is
      It : Natural := Natural (Seconds);
   begin
      Sub_Second := 0.0;
      Second := It mod 60;
      It := It / 60;
      Minute := It mod 60;
      It := It / 60;
      Hour := It;
   end Split;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date       : Time;
      Year       : out Year_Number;
      Month      : out Month_Number;
      Day        : out Day_Number;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration)
   is
      D : Duration;
   begin
      Split (Date, Year, Month, Day, D);
      Split (D, Hour, Minute, Second, Sub_Second);
   end Split;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date          : Time;
      Period        : Duration;
      Cycle_Count   : out Natural;
      Partial_Cycle : out Duration)
   is
      P : constant Positive := Natural'Max (Natural (Period), 1);
   begin
      Cycle_Count := Natural (Date) / P;
      Partial_Cycle := Duration (Natural (Date) mod P);
   end Split;

   -----------
   -- Start --
   -----------

   function Start return Time is
   begin
      return 0;
   end Start;

   ----------------
   -- Start_Year --
   ----------------

   function Start_Year return Year_Number is
   begin
      return 4551;
   end Start_Year;

   ----------------
   -- Time_Image --
   ----------------

   function Time_Image
     (Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration := 0.0)
      return String
   is
      pragma Unreferenced (Sub_Second);
      H : constant String := Integer'Image (Hour + 100);
      M : constant String := Integer'Image (Minute + 100);
      S : constant String := Integer'Image (Second + 100);
   begin
      return H (3 .. 4) & ":" & M (3 .. 4) & ":" & S (3 .. 4);
   end Time_Image;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0)
      return Time
   is
   begin
      return T : Time := Time (Year) do
         T := T * 12 + Time (Month);
         T := T * 30 + Time (Day);
         T := T * 86_400 + Time (Seconds);
      end return;
   end Time_Of;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration := 0.0)
      return Time
   is
   begin
      return Time_Of
        (Year, Month, Day, Seconds_Of (Hour, Minute, Second, Sub_Second));
   end Time_Of;

   ----------
   -- Year --
   ----------

   function Year (Date : Time) return Year_Number is
   begin
      return Convert (Date, Years);
   end Year;

end Carthage.Calendar;
