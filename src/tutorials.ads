with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_WebGL;
with Gnoga.Gui.View;

package Tutorials is

   type Root_Tutorial_Type is abstract tagged limited private;

   function Name (Tutorial : Root_Tutorial_Type) return String
                  is abstract;

   procedure Start
     (Tutorial : in out Root_Tutorial_Type;
      View     : in out Gnoga.Gui.View.View_Base_Type'Class)
   is abstract;

   procedure Stop
     (Tutorial : in out Root_Tutorial_Type)
   is abstract;

   procedure Create_Canvas
     (Tutorial : in out Root_Tutorial_Type'Class;
      View     : in out Gnoga.Gui.View.View_Base_Type'Class);

   procedure Delete_Canvas
     (Tutorial : in out Root_Tutorial_Type'Class);

   type Tutorial_Access is access all Root_Tutorial_Type'Class;

private

   use Gnoga.Gui.Element.Canvas;
   use Gnoga.Gui.Element.Canvas.Context_WebGL;

   type Root_Tutorial_Type is abstract tagged limited
      record
         Canvas   : Canvas_Access;
         Context  : Context_WebGL_Access;
      end record;

end Tutorials;
