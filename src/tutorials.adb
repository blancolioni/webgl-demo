with Ada.Unchecked_Deallocation;

package body Tutorials is

   -------------------
   -- Create_Canvas --
   -------------------

   procedure Create_Canvas
     (Tutorial : in out Root_Tutorial_Type'Class;
      View     : in out Gnoga.Gui.View.View_Base_Type'Class)
   is
   begin
      Tutorial.Canvas := new Canvas_Type;
      Tutorial.Canvas.Create (View, 640, 480);
      Tutorial.Context := new Context_WebGL_Type;
      Tutorial.Context.Get_Drawing_Context_WebGL (Tutorial.Canvas.all);
   end Create_Canvas;

   -------------------
   -- Delete_Canvas --
   -------------------

   procedure Delete_Canvas
     (Tutorial : in out Root_Tutorial_Type'Class)
   is
      procedure Free_Context is
        new Ada.Unchecked_Deallocation
          (Context_WebGL_Type, Context_WebGL_Access);
   begin
      Tutorial.Canvas.Hidden (True);
      Free_Context (Tutorial.Context);
      Tutorial.Canvas.Free;
   end Delete_Canvas;

end Tutorials;
