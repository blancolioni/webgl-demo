package body Tutorials.Clear_Screen_To_Black is

   type Tutorial_Type is
     new Root_Tutorial_Type with null record;

   overriding function Name
     (Tutorial : Tutorial_Type)
      return String;

   overriding procedure Start
     (Tutorial : in out Tutorial_Type;
      View     : in out Gnoga.Gui.View.View_Base_Type'Class);

   overriding procedure Stop
     (Tutorial : in out Tutorial_Type);

   ----------
   -- Name --
   ----------

   overriding function Name
     (Tutorial : Tutorial_Type)
      return String
   is
      pragma Unreferenced (Tutorial);
   begin
      return "Clear screen to black";
   end Name;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Tutorial : in out Tutorial_Type;
      View     : in out Gnoga.Gui.View.View_Base_Type'Class)
   is
      pragma Unreferenced (View);
   begin
      Tutorial.Context.Clear_Color (0.0, 0.0, 0.0, 1.0);
      Tutorial.Context.Clear ((1 => GL_COLOR_BUFFER_BIT));
   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (Tutorial : in out Tutorial_Type)
   is
   begin
      null;
   end Stop;

   --------------
   -- Tutorial --
   --------------

   function Tutorial return Tutorial_Access is
   begin
      return new Tutorial_Type;
   end Tutorial;

end Tutorials.Clear_Screen_To_Black;
