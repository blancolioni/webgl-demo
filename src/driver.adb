with Gnoga.Application.Multi_Connect;

with WebGL_Demo;

procedure Driver is
begin
   Gnoga.Application.Title ("WebGL Demo");

   WebGL_Demo.Create_Tutorials;

   Gnoga.Application.HTML_On_Close ("Application ended.");

   Gnoga.Application.Multi_Connect.Initialize;

   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Event => WebGL_Demo.On_Connect'Access,
      Path  => "default");

   Gnoga.Application.Multi_Connect.Message_Loop;
end Driver;
