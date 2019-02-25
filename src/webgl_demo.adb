with Ada.Containers.Vectors;

with Gnoga.Types;
with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;

with Tutorials.Clear_Screen_To_Black;
with Tutorials.Draw_White_Square;
with Tutorials.Draw_Color_Square;
with Tutorials.Rotating_Square;
with Tutorials.Rotating_Cube;
with Tutorials.Textured_Cube;
with Tutorials.Lit_Cube;

package body WebGL_Demo is

   type Tutorial_Button_Type is
     new Gnoga.Gui.Element.Common.Button_Type with
      record
         Tutorial : Positive;
      end record;

   type Pointer_To_Tutorial_Button_Class is
     access all Tutorial_Button_Type;

   package Tutorial_Button_Vectors is
     new Ada.Containers.Vectors (Positive, Pointer_To_Tutorial_Button_Class);

   type App_Data is new Gnoga.Types.Connection_Data_Type with
      record
         My_View          : Gnoga.Gui.View.View_Type;
         Tutorial_Buttons : Tutorial_Button_Vectors.Vector;
         Current_Tutorial : Tutorials.Tutorial_Access;
         Started          : Boolean := False;
      end record;

   type App_Access is access all App_Data;

   package Tutorial_Vectors is
     new Ada.Containers.Vectors
       (Positive, Tutorials.Tutorial_Access, Tutorials."=");

   Tutorial_Vector : Tutorial_Vectors.Vector;

   procedure On_Tutorial (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   ----------------------
   -- Create_Tutorials --
   ----------------------

   procedure Create_Tutorials is
   begin
      Tutorial_Vector.Append (Tutorials.Clear_Screen_To_Black.Tutorial);
      Tutorial_Vector.Append (Tutorials.Draw_White_Square.Tutorial);
      Tutorial_Vector.Append (Tutorials.Draw_Color_Square.Tutorial);
      Tutorial_Vector.Append (Tutorials.Rotating_Square.Tutorial);
      Tutorial_Vector.Append (Tutorials.Rotating_Cube.Tutorial);
      Tutorial_Vector.Append (Tutorials.Textured_Cube.Tutorial);
      Tutorial_Vector.Append (Tutorials.Lit_Cube.Tutorial);
   end Create_Tutorials;

   ----------------
   -- On_Connect --
   ----------------

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App : constant App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);

      App.My_View.Create (Main_Window);

      for I in 1 .. Tutorial_Vector.Last_Index loop
         declare
            Button : constant Pointer_To_Tutorial_Button_Class :=
                       new Tutorial_Button_Type'
                         (Gnoga.Gui.Element.Common.Button_Type with
                          Tutorial => I);
         begin
            Button.Create (App.My_View, Tutorial_Vector.Element (I).Name);
            Button.On_Click_Handler (On_Tutorial'Access);
            App.Tutorial_Buttons.Append (Button);
         end;
      end loop;

      App.My_View.Horizontal_Rule;

   end On_Connect;

   -----------------
   -- On_Tutorial --
   -----------------

   procedure On_Tutorial (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Button : Tutorial_Button_Type renames Tutorial_Button_Type (Object);
      App    : constant App_Access := App_Access (Object.Connection_Data);
      Tutorial : constant Tutorials.Tutorial_Access :=
                   Tutorial_Vector.Element (Button.Tutorial);
   begin
      if App.Started then
         App.Current_Tutorial.Stop;
         App.Current_Tutorial.Delete_Canvas;
         App.Started := False;
      end if;

      Tutorial.Create_Canvas (App.My_View);
      Tutorial.Start (App.My_View);
      App.Current_Tutorial := Tutorial;
      App.Started := True;
   end On_Tutorial;

end WebGL_Demo;
