with Ada.Unchecked_Deallocation;

package body Tutorials.Rotating_Square is

   type State_Type is
      record
         Context : Context_WebGL_Access;
         Program           : GLuint;
         Vertex_Position   : GLuint;
         Vertex_Color      : GLuint;
         Projection_Matrix : GLuint;
         Model_View_Matrix : GLuint;
         Position_Buffer   : GLuint;
         Color_Buffer      : GLuint;
         Rotation          : GLfloat := 0.0;
         Aspect_Ratio      : GLfloat;
      end record;

   task type Render_Task is
      entry Start (Initial_State : State_Type);
      entry Stop;
   end Render_Task;

   type Render_Task_Access is access Render_Task;

   procedure Render_State
     (State   : State_Type);

   type Tutorial_Type is
     new Root_Tutorial_Type with
      record
         State    : State_Type;
         Renderer : Render_Task_Access;
      end record;

   overriding function Name
     (Tutorial : Tutorial_Type)
      return String;

   overriding procedure Start
     (Tutorial : in out Tutorial_Type;
      View     : in out Gnoga.Gui.View.View_Base_Type'Class);

   overriding procedure Stop
     (Tutorial : in out Tutorial_Type);

   Vertex_Shader_Source : constant String :=
                            "attribute vec4 vertexPosition;"
                            & "attribute vec4 vertexColor;"
                            & "uniform mat4 modelViewMatrix;"
                            & "uniform mat4 projectionMatrix;"
                            & "varying lowp vec4 color;"
                            & "void main() {"
                            & "  gl_Position = projectionMatrix "
                            & " * modelViewMatrix * vertexPosition;"
                            & "  color = vertexColor;"
                            & "}";

   Fragment_Shader_Source : constant String :=
                              "varying lowp vec4 color;"
                              & "void main() {"
                              & "  gl_FragColor = color;"
                              & "}";

   function Load_Shader
     (Context  : Context_WebGL_Access;
      Shader_T : Shader_Type;
      Source   : String)
      return GLuint;

   function Initialize_Shader_Program
     (Context                : Context_WebGL_Access;
      Vertex_Shader_Source   : String;
      Fragment_Shader_Source : String)
      return GLuint;

   -------------------------------
   -- Initialize_Shader_Program --
   -------------------------------

   function Initialize_Shader_Program
     (Context                : Context_WebGL_Access;
      Vertex_Shader_Source   : String;
      Fragment_Shader_Source : String)
      return GLuint
   is
      Vertex_Shader : constant GLuint :=
                        Load_Shader (Context, GL_Vertex_Shader,
                                     Vertex_Shader_Source);
      Fragment_Shader : constant GLuint :=
                          Load_Shader (Context, GL_Fragment_Shader,
                                       Fragment_Shader_Source);
      Program         : constant GLuint := Context.Create_Program;
   begin
      Context.Attach_Shader (Program, Vertex_Shader);
      Context.Attach_Shader (Program, Fragment_Shader);
      Context.Link_Program (Program);
      if not Context.Get_Program_Parameter (Program, GL_Link_Status) then
         raise Program_Error with "link error";
      end if;
      return Program;
   end Initialize_Shader_Program;

   -----------------
   -- Load_Shader --
   -----------------

   function Load_Shader
     (Context  : Context_WebGL_Access;
      Shader_T : Shader_Type;
      Source   : String)
      return GLuint
   is
      Shader : constant GLuint := Context.Create_Shader (Shader_T);
   begin
      Context.Shader_Source (Shader, Source);
      Context.Compile_Shader (Shader);
      if not Context.Get_Shader_Parameter (Shader, GL_Compile_Status) then
         raise Program_Error with "an error occurred compilng the shader";
      end if;
      return Shader;
   end Load_Shader;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Tutorial : Tutorial_Type)
      return String
   is
      pragma Unreferenced (Tutorial);
   begin
      return "Rotating Square";
   end Name;

   ------------------
   -- Render_State --
   ------------------

   procedure Render_State
     (State : State_Type)
   is
      Context : constant Context_WebGL_Access := State.Context;
      Model_View_Matrix : Matrix_4 := Matrices.Unit_Matrix (4);
      Projection_Matrix : Matrix_4;
   begin
      Context.Begin_Render;
      Context.Perspective
        (Projection_Matrix, 45.0, State.Aspect_Ratio, 0.1, 100.0);
      Context.Translate (Model_View_Matrix, 0.0, 0.0, -6.0);
      Context.Rotate (Model_View_Matrix, State.Rotation, 0.0, 0.0, 1.0);

      Context.Clear ((GL_Color_Buffer_Bit, GL_Depth_Buffer_Bit));

      Context.Bind_Buffer (GL_Array_Buffer, State.Position_Buffer);
      Context.Vertex_Attrib_Pointer
        (State.Vertex_Position, 2,
         GL_Float, False, 0, 0);
      Context.Enable_Vertex_Attrib_Array (State.Vertex_Position);

      Context.Bind_Buffer (GL_Array_Buffer, State.Color_Buffer);
      Context.Vertex_Attrib_Pointer
        (State.Vertex_Color, 4,
         GL_Float, False, 0, 0);
      Context.Enable_Vertex_Attrib_Array (State.Vertex_Color);

      Context.Use_Program (State.Program);
      Context.Uniform_Matrix
        (State.Projection_Matrix, Projection_Matrix);
      Context.Uniform_Matrix
        (State.Model_View_Matrix, Model_View_Matrix);
      Context.Draw_Arrays (GL_Triangle_Strip, 0, 4);
      Context.End_Render;
   end Render_State;

   -----------------
   -- Render_Task --
   -----------------

   task body Render_Task is
      State : State_Type;
   begin
      accept Start (Initial_State : in State_Type) do
         State := Initial_State;
      end Start;

      loop
         select
            accept Stop;
            exit;
         else
            delay 0.02;
            Render_State (State);
            State.Rotation := State.Rotation + 0.2;
         end select;
      end loop;
   end Render_Task;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Tutorial : in out Tutorial_Type;
      View     : in out Gnoga.Gui.View.View_Base_Type'Class)
   is
      pragma Unreferenced (View);
      Context : constant Context_WebGL_Access := Tutorial.Context;
   begin

      Tutorial.State.Program :=
        Initialize_Shader_Program
          (Context                => Context,
           Vertex_Shader_Source   => Vertex_Shader_Source,
           Fragment_Shader_Source => Fragment_Shader_Source);
      Tutorial.State.Vertex_Position :=
        Context.Get_Attrib_Location
          (Tutorial.State.Program, "vertexPosition");

      Tutorial.State.Vertex_Color :=
        Context.Get_Attrib_Location
          (Tutorial.State.Program, "vertexColor");

      Tutorial.State.Projection_Matrix :=
        Context.Get_Uniform_Location
          (Tutorial.State.Program, "projectionMatrix");
      Tutorial.State.Model_View_Matrix :=
        Context.Get_Uniform_Location
          (Tutorial.State.Program, "modelViewMatrix");

      Tutorial.State.Position_Buffer := Context.Create_Buffer;
      Context.Bind_Buffer
        (GL_Array_Buffer, Tutorial.State.Position_Buffer);

      Context.Buffer_Data
        (Target => GL_Array_Buffer,
         Data   => Float_Array'(-1.0, 1.0, 1.0, 1.0, -1.0, -1.0, 1.0, -1.0),
         Usage  => GL_Static_Draw);

      Tutorial.State.Color_Buffer := Context.Create_Buffer;
      Context.Bind_Buffer (GL_Array_Buffer, Tutorial.State.Color_Buffer);
      Context.Buffer_Data (GL_Array_Buffer,
                           Float_Array'
                             (1.0,  1.0,  1.0,  1.0,    -- white
                              1.0,  0.0,  0.0,  1.0,    -- red
                              0.0,  1.0,  0.0,  1.0,    -- green
                              0.0,  0.0,  1.0,  1.0    -- blue
                             ),
                           GL_Static_Draw);

      Context.Clear_Color (0.0, 0.0, 0.0, 1.0);
      Context.Clear_Depth (1.0);
      Context.Enable (GL_Depth_Test);
      Context.Depth_Func (GL_Lequal);

      Tutorial.State.Aspect_Ratio :=
        GLfloat (Tutorial.Canvas.Width)
        / GLfloat (Tutorial.Canvas.Height);

      Tutorial.State.Context := Context;

      Tutorial.Renderer := new Render_Task;
      Tutorial.Renderer.Start (Tutorial.State);

   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (Tutorial : in out Tutorial_Type)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation (Render_Task, Render_Task_Access);
   begin
      Tutorial.Renderer.Stop;
      Free (Tutorial.Renderer);
   end Stop;

   --------------
   -- Tutorial --
   --------------

   function Tutorial return Tutorial_Access is
   begin
      return new Tutorial_Type;
   end Tutorial;

end Tutorials.Rotating_Square;
