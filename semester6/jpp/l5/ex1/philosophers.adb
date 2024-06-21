with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Philosophers is
   Number_Of_Philosophers : constant := 10;
   Meal_Count : constant := 20;
   Min_Delay : constant Duration := 0.020;
   Max_Delay : constant Duration := 0.100;

   protected type Fork is
      entry Grab;
      procedure Put_Down;
   private
      Seized : Boolean := False;
   end Fork;

   protected body Fork is
      entry Grab when not Seized is
      begin
         Seized := True;
      end Grab;
      
      procedure Put_Down is
      begin
         Seized := False;
      end Put_Down;
   end Fork;

   task type Person (ID : Positive; First, Second : not null access Fork);

   task body Person is
      Dice : Generator;
   begin
      Reset (Dice);
      for Life_Cycle in 1..Meal_Count loop
         Put_Line ("Philosopher" & Positive'Image (ID) & " is thinking");
         delay Min_Delay + Duration (Duration (Random (Dice) ) * Duration (Max_Delay - Min_Delay));

         Put_Line ("Philosopher" & Positive'Image (ID) & " is hungry");
         First.Grab;
         Second.Grab;

         Put_Line ("Philosopher" & Positive'Image (ID) & " is eating");
         delay Min_Delay + Duration (Duration ( Random (Dice) ) * Duration (Max_Delay - Min_Delay));
         Second.Put_Down;
         First.Put_Down;
      end loop;
      Put_Line ("Philosopher" & Positive'Image (ID) & " finished all his meals");
   end Person;

   Forks : array (1..Number_Of_Philosophers) of aliased Fork;

   type Person_Access is access Person;
   Philosophers_Array : array (1..Number_Of_Philosophers) of Person_Access;

begin
   for I in 1..Number_Of_Philosophers loop
      if I = Number_Of_Philosophers then
         Philosophers_Array(I) := new Person (I, Forks(I)'Access, Forks(1)'Access);
      else
         Philosophers_Array(I) := new Person (I, Forks(I)'Access, Forks(I + 1)'Access);
      end if;
   end loop;

   null;
end Philosophers;
