with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;

package WebGL_Demo is

   procedure Create_Tutorials;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Setup GUI for each connection.

end WebGL_Demo;
