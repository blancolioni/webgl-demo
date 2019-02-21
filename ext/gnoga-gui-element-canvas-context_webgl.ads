with System;
with Ada.Numerics.Generic_Real_Arrays;
private with Ada.Containers.Indefinite_Vectors;

package Gnoga.Gui.Element.Canvas.Context_WebGL is

   pragma Style_Checks (Off);

   type GLuchar is mod 2 ** 8;
   type GLchar is range -128 .. 127;
   type GLint8 is range -2 ** 15 .. 2 ** 15 - 1;
   type GLuint8 is mod 2 ** 16;
   type GLint16 is range -2 ** 15 .. 2 ** 15 - 1;
   type GLuint16 is mod 2 ** 16;
   type GLint is range -2 ** 31 .. 2 ** 31 - 1;
   type GLuint is mod 2 **32;
   type GLenum is new GLint;
   type GLfloat is new Float;
   type GLsizei is new GLuint;
   type GLintptr is new GLuint;
   type GLvoidptr is new System.Address;

   type Float_Array is array (Positive range <>) of GLfloat;
   type Uint16_Array is array (Positive range <>) of GLuint16;
   type Uint8_Array is array (Positive range <>) of GLuint8;
   subtype Element_Array is Uint16_Array;
   type CheckedInt32 is array (Positive range <>) of GLint;
   type ColorF is new GLfloat;
   type Texture is new GLuint;
   type BufferOffset is access GLint;
   package Matrices is
     new Ada.Numerics.Generic_Real_Arrays (GLfloat);

   subtype Matrix_4 is
     Matrices.Real_Matrix (1 .. 4, 1 .. 4);

   subtype Matrix_3 is
     Matrices.Real_Matrix (1 .. 3, 1 .. 3);

   subtype Vector_4 is
     Matrices.Real_Vector (1 .. 4);

   subtype Vector_3 is
     Matrices.Real_Vector (1 .. 3);


   type Attrib_Mask is
     (GL_DEPTH_BUFFER_BIT,
      GL_STENCIL_BUFFER_BIT,
      GL_COLOR_BUFFER_BIT);
   type Attrib_Mask_Array is array (Positive range <>) of Attrib_Mask;

   type Alpha_Function is
     (GL_NEVER,
      GL_LESS,
      GL_EQUAL,
      GL_LEQUAL,
      GL_GREATER,
      GL_NOTEQUAL,
      GL_GEQUAL,
      GL_ALWAYS);

   type Blend_Equation_Mode_EXT is
     (GL_FUNC_ADD,
      GL_FUNC_SUBTRACT,
      GL_FUNC_REVERSE_SUBTRACT);

   type Buffer_Target_ARB is
     (GL_ARRAY_BUFFER,
      GL_ELEMENT_ARRAY_BUFFER);

   type Buffer_Usage_ARB is
     (GL_STREAM_DRAW,
      GL_STATIC_DRAW,
      GL_DYNAMIC_DRAW);

   type Clear_Buffer_Mask is
     (GL_DEPTH_BUFFER_BIT,
      GL_STENCIL_BUFFER_BIT,
      GL_COLOR_BUFFER_BIT);
   type Clear_Buffer_Mask_Array is array (Positive range <>) of Clear_Buffer_Mask;

   type Color_Material_Face is
     (GL_FRONT,
      GL_BACK,
      GL_FRONT_AND_BACK);

   type Color_Pointer_Type is
     (GL_BYTE,
      GL_UNSIGNED_BYTE,
      GL_SHORT,
      GL_UNSIGNED_SHORT,
      GL_INT,
      GL_UNSIGNED_INT,
      GL_FLOAT);

   type Cull_Face_Mode is
     (GL_FRONT,
      GL_BACK,
      GL_FRONT_AND_BACK);

   type Depth_Function is
     (GL_NEVER,
      GL_LESS,
      GL_EQUAL,
      GL_LEQUAL,
      GL_GREATER,
      GL_NOTEQUAL,
      GL_GEQUAL,
      GL_ALWAYS);

   type Draw_Buffer_Mode is
     (GL_NONE,
      GL_FRONT,
      GL_BACK,
      GL_FRONT_AND_BACK);

   type Draw_Elements_Type is
     (GL_UNSIGNED_BYTE,
      GL_UNSIGNED_SHORT,
      GL_UNSIGNED_INT);

   type Enable_Cap is
     (GL_CULL_FACE,
      GL_DEPTH_TEST,
      GL_STENCIL_TEST,
      GL_DITHER,
      GL_BLEND,
      GL_SCISSOR_TEST,
      GL_TEXTURE_2D,
      GL_POLYGON_OFFSET_FILL,
      GL_SAMPLE_ALPHA_TO_COVERAGE,
      GL_SAMPLE_COVERAGE);

   type Error_Code is
     (GL_NO_ERROR,
      GL_INVALID_ENUM,
      GL_INVALID_VALUE,
      GL_INVALID_OPERATION,
      GL_OUT_OF_MEMORY,
      GL_INVALID_FRAMEBUFFER_OPERATION);

   type Fog_Coordinate_Pointer_Type is
     (GL_FLOAT);

   type Fog_Mode is
     (GL_LINEAR);

   type Fog_Pointer_Type_EXT is
     (GL_FLOAT);

   type Fog_Pointer_Type_IBM is
     (GL_FLOAT);

   type Front_Face_Direction is
     (GL_CW,
      GL_CCW);

   type Get_PName is
     (GL_LINE_WIDTH,
      GL_CULL_FACE,
      GL_CULL_FACE_MODE,
      GL_FRONT_FACE,
      GL_DEPTH_RANGE,
      GL_DEPTH_TEST,
      GL_DEPTH_WRITEMASK,
      GL_DEPTH_CLEAR_VALUE,
      GL_DEPTH_FUNC,
      GL_STENCIL_TEST,
      GL_STENCIL_CLEAR_VALUE,
      GL_STENCIL_FUNC,
      GL_STENCIL_VALUE_MASK,
      GL_STENCIL_FAIL,
      GL_STENCIL_PASS_DEPTH_FAIL,
      GL_STENCIL_PASS_DEPTH_PASS,
      GL_STENCIL_REF,
      GL_STENCIL_WRITEMASK,
      GL_VIEWPORT,
      GL_DITHER,
      GL_BLEND,
      GL_SCISSOR_BOX,
      GL_SCISSOR_TEST,
      GL_COLOR_CLEAR_VALUE,
      GL_COLOR_WRITEMASK,
      GL_UNPACK_ALIGNMENT,
      GL_PACK_ALIGNMENT,
      GL_MAX_TEXTURE_SIZE,
      GL_MAX_VIEWPORT_DIMS,
      GL_SUBPIXEL_BITS,
      GL_RED_BITS,
      GL_GREEN_BITS,
      GL_BLUE_BITS,
      GL_ALPHA_BITS,
      GL_DEPTH_BITS,
      GL_STENCIL_BITS,
      GL_TEXTURE_2D,
      GL_POLYGON_OFFSET_UNITS,
      GL_BLEND_COLOR,
      GL_BLEND_EQUATION_RGB,
      GL_POLYGON_OFFSET_FILL,
      GL_POLYGON_OFFSET_FACTOR,
      GL_TEXTURE_BINDING_2D,
      GL_SAMPLE_BUFFERS,
      GL_SAMPLES,
      GL_SAMPLE_COVERAGE_VALUE,
      GL_SAMPLE_COVERAGE_INVERT,
      GL_BLEND_DST_RGB,
      GL_BLEND_SRC_RGB,
      GL_BLEND_DST_ALPHA,
      GL_BLEND_SRC_ALPHA,
      GL_ALIASED_POINT_SIZE_RANGE,
      GL_ALIASED_LINE_WIDTH_RANGE,
      GL_ACTIVE_TEXTURE,
      GL_MAX_RENDERBUFFER_SIZE,
      GL_TEXTURE_BINDING_CUBE_MAP,
      GL_MAX_CUBE_MAP_TEXTURE_SIZE,
      GL_NUM_COMPRESSED_TEXTURE_FORMATS,
      GL_COMPRESSED_TEXTURE_FORMATS,
      GL_STENCIL_BACK_FUNC,
      GL_STENCIL_BACK_FAIL,
      GL_STENCIL_BACK_PASS_DEPTH_FAIL,
      GL_STENCIL_BACK_PASS_DEPTH_PASS,
      GL_BLEND_EQUATION_ALPHA,
      GL_MAX_VERTEX_ATTRIBS,
      GL_MAX_TEXTURE_IMAGE_UNITS,
      GL_ARRAY_BUFFER_BINDING,
      GL_ELEMENT_ARRAY_BUFFER_BINDING,
      GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS,
      GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS,
      GL_CURRENT_PROGRAM,
      GL_IMPLEMENTATION_COLOR_READ_TYPE,
      GL_IMPLEMENTATION_COLOR_READ_FORMAT,
      GL_STENCIL_BACK_REF,
      GL_STENCIL_BACK_VALUE_MASK,
      GL_STENCIL_BACK_WRITEMASK,
      GL_RENDERBUFFER_BINDING,
      GL_NUM_SHADER_BINARY_FORMATS,
      GL_SHADER_COMPILER,
      GL_MAX_VERTEX_UNIFORM_VECTORS,
      GL_MAX_VARYING_VECTORS,
      GL_MAX_FRAGMENT_UNIFORM_VECTORS);

   type Get_Texture_Parameter is
     (GL_TEXTURE_MAG_FILTER,
      GL_TEXTURE_MIN_FILTER,
      GL_TEXTURE_WRAP_S,
      GL_TEXTURE_WRAP_T);

   type Hint_Mode is
     (GL_DONT_CARE,
      GL_FASTEST,
      GL_NICEST);

   type Hint_Target is
     (GL_GENERATE_MIPMAP_HINT);

   type Index_Pointer_Type is
     (GL_SHORT,
      GL_INT,
      GL_FLOAT);

   type Light_Env_Mode_SGIX is
     (GL_REPLACE);

   type List_Name_Type is
     (GL_BYTE,
      GL_UNSIGNED_BYTE,
      GL_SHORT,
      GL_UNSIGNED_SHORT,
      GL_INT,
      GL_UNSIGNED_INT,
      GL_FLOAT);

   type Logic_Op is
     (GL_INVERT);

   type Material_Face is
     (GL_FRONT,
      GL_BACK,
      GL_FRONT_AND_BACK);

   type Matrix_Mode is
     (GL_TEXTURE);

   type Normal_Pointer_Type is
     (GL_BYTE,
      GL_SHORT,
      GL_INT,
      GL_FLOAT);

   type Pixel_Format is
     (GL_UNSIGNED_SHORT,
      GL_UNSIGNED_INT,
      GL_DEPTH_COMPONENT,
      GL_ALPHA,
      GL_RGB,
      GL_RGBA,
      GL_LUMINANCE,
      GL_LUMINANCE_ALPHA);

   type Internal_Format is
     (GL_DEPTH_COMPONENT,
      GL_RGB,
      GL_RGBA,
      GL_RGBA4,
      GL_RGB5_A1,
      GL_DEPTH_COMPONENT16);

   type Pixel_Store_Parameter is
     (GL_UNPACK_ALIGNMENT,
      GL_PACK_ALIGNMENT);

   type Pixel_Tex_Gen_Mode is
     (GL_NONE,
      GL_RGB,
      GL_RGBA,
      GL_LUMINANCE,
      GL_LUMINANCE_ALPHA);

   type Pixel_Type is
     (GL_BYTE,
      GL_UNSIGNED_BYTE,
      GL_SHORT,
      GL_UNSIGNED_SHORT,
      GL_INT,
      GL_UNSIGNED_INT,
      GL_FLOAT,
      GL_UNSIGNED_SHORT_4_4_4_4,
      GL_UNSIGNED_SHORT_5_5_5_1);

   type Primitive_Type is
     (GL_POINTS,
      GL_LINES,
      GL_LINE_LOOP,
      GL_LINE_STRIP,
      GL_TRIANGLES,
      GL_TRIANGLE_STRIP,
      GL_TRIANGLE_FAN);

   type Read_Buffer_Mode is
     (GL_FRONT,
      GL_BACK);

   type Stencil_Face_Direction is
     (GL_FRONT,
      GL_BACK,
      GL_FRONT_AND_BACK);

   type Stencil_Function is
     (GL_NEVER,
      GL_LESS,
      GL_EQUAL,
      GL_LEQUAL,
      GL_GREATER,
      GL_NOTEQUAL,
      GL_GEQUAL,
      GL_ALWAYS);

   type Stencil_Op is
     (GL_ZERO,
      GL_INVERT,
      GL_KEEP,
      GL_REPLACE,
      GL_INCR,
      GL_DECR,
      GL_INCR_WRAP,
      GL_DECR_WRAP);

   type String_Name is
     (GL_VENDOR,
      GL_RENDERER,
      GL_VERSION,
      GL_EXTENSIONS,
      GL_SHADING_LANGUAGE_VERSION);

   type Tex_Coord_Pointer_Type is
     (GL_SHORT,
      GL_INT,
      GL_FLOAT);

   type Texture_Env_Mode is
     (GL_BLEND);

   type Texture_Mag_Filter is
     (GL_NEAREST,
      GL_LINEAR);

   type Texture_Min_Filter is
     (GL_NEAREST,
      GL_LINEAR,
      GL_NEAREST_MIPMAP_NEAREST,
      GL_LINEAR_MIPMAP_NEAREST,
      GL_NEAREST_MIPMAP_LINEAR,
      GL_LINEAR_MIPMAP_LINEAR);

   type Texture_Parameter_Name is
     (GL_TEXTURE_MAG_FILTER,
      GL_TEXTURE_MIN_FILTER,
      GL_TEXTURE_WRAP_S,
      GL_TEXTURE_WRAP_T);

   type Texture_Target is
     (GL_TEXTURE_2D,
      GL_TEXTURE_CUBE_MAP,
      GL_TEXTURE_CUBE_MAP_POSITIVE_X,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Z);

   type Texture_Wrap_Mode is
     (GL_REPEAT,
      GL_CLAMP_TO_EDGE);

   type Vertex_Pointer_Type is
     (GL_SHORT,
      GL_INT,
      GL_FLOAT);

   type Framebuffer_Attachment is
     (GL_COLOR_ATTACHMENT0,
      GL_DEPTH_ATTACHMENT);

   type Renderbuffer_Target is
     (GL_RENDERBUFFER);

   type Framebuffer_Target is
     (GL_FRAMEBUFFER);

   type Texture_Unit is
     (GL_TEXTURE0,
      GL_TEXTURE1,
      GL_TEXTURE2,
      GL_TEXTURE3,
      GL_TEXTURE4,
      GL_TEXTURE5,
      GL_TEXTURE6,
      GL_TEXTURE7,
      GL_TEXTURE8,
      GL_TEXTURE9,
      GL_TEXTURE10,
      GL_TEXTURE11,
      GL_TEXTURE12,
      GL_TEXTURE13,
      GL_TEXTURE14,
      GL_TEXTURE15,
      GL_TEXTURE16,
      GL_TEXTURE17,
      GL_TEXTURE18,
      GL_TEXTURE19,
      GL_TEXTURE20,
      GL_TEXTURE21,
      GL_TEXTURE22,
      GL_TEXTURE23,
      GL_TEXTURE24,
      GL_TEXTURE25,
      GL_TEXTURE26,
      GL_TEXTURE27,
      GL_TEXTURE28,
      GL_TEXTURE29,
      GL_TEXTURE30,
      GL_TEXTURE31);

   type Framebuffer_Status is
     (GL_FRAMEBUFFER_COMPLETE,
      GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT,
      GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT,
      GL_FRAMEBUFFER_UNSUPPORTED);

   type Graphics_Reset_Status is
     (GL_NO_ERROR);

   type Path_Fill_Mode is
     (GL_INVERT);

   type Vertex_Buffer_Object_Parameter is
     (GL_BUFFER_SIZE,
      GL_BUFFER_USAGE);

   type Renderbuffer_Parameter_Name is
     (GL_RENDERBUFFER_WIDTH,
      GL_RENDERBUFFER_HEIGHT,
      GL_RENDERBUFFER_INTERNAL_FORMAT,
      GL_RENDERBUFFER_RED_SIZE,
      GL_RENDERBUFFER_GREEN_SIZE,
      GL_RENDERBUFFER_BLUE_SIZE,
      GL_RENDERBUFFER_ALPHA_SIZE,
      GL_RENDERBUFFER_DEPTH_SIZE,
      GL_RENDERBUFFER_STENCIL_SIZE);

   type Vertex_Buffer_Object_Usage is
     (GL_STREAM_DRAW,
      GL_STATIC_DRAW,
      GL_DYNAMIC_DRAW);

   type Blending_Factor is
     (GL_ZERO,
      GL_ONE,
      GL_SRC_COLOR,
      GL_ONE_MINUS_SRC_COLOR,
      GL_SRC_ALPHA,
      GL_ONE_MINUS_SRC_ALPHA,
      GL_DST_ALPHA,
      GL_ONE_MINUS_DST_ALPHA,
      GL_DST_COLOR,
      GL_ONE_MINUS_DST_COLOR,
      GL_SRC_ALPHA_SATURATE,
      GL_CONSTANT_COLOR,
      GL_ONE_MINUS_CONSTANT_COLOR,
      GL_CONSTANT_ALPHA,
      GL_ONE_MINUS_CONSTANT_ALPHA);

   type Blit_Framebuffer_Filter is
     (GL_NEAREST,
      GL_LINEAR);

   type Buffer_Storage_Target is
     (GL_ARRAY_BUFFER,
      GL_ELEMENT_ARRAY_BUFFER);

   type Check_Framebuffer_Status_Target is
     (GL_FRAMEBUFFER);

   type Copy_Buffer_Sub_Data_Target is
     (GL_ARRAY_BUFFER,
      GL_ELEMENT_ARRAY_BUFFER);

   type Shader_Type is
     (GL_FRAGMENT_SHADER,
      GL_VERTEX_SHADER);

   type Debug_Source is
     (GL_DONT_CARE);

   type Debug_Type is
     (GL_DONT_CARE);

   type Debug_Severity is
     (GL_DONT_CARE);

   type Sampler_Parameter_Name is
     (GL_TEXTURE_MAG_FILTER,
      GL_TEXTURE_MIN_FILTER,
      GL_TEXTURE_WRAP_S,
      GL_TEXTURE_WRAP_T);

   type Object_Identifier is
     (GL_TEXTURE,
      GL_FRAMEBUFFER,
      GL_RENDERBUFFER);

   type Color_Buffer is
     (GL_NONE,
      GL_FRONT,
      GL_BACK,
      GL_FRONT_AND_BACK,
      GL_COLOR_ATTACHMENT0);

   type Vertex_Array_PName is
     (GL_VERTEX_ATTRIB_ARRAY_ENABLED,
      GL_VERTEX_ATTRIB_ARRAY_SIZE,
      GL_VERTEX_ATTRIB_ARRAY_STRIDE,
      GL_VERTEX_ATTRIB_ARRAY_TYPE,
      GL_VERTEX_ATTRIB_ARRAY_NORMALIZED);

   type Shader_Parameter_Name is
     (GL_SHADER_TYPE,
      GL_DELETE_STATUS,
      GL_COMPILE_STATUS,
      GL_INFO_LOG_LENGTH,
      GL_SHADER_SOURCE_LENGTH);

   type Pipeline_Parameter_Name is
     (GL_FRAGMENT_SHADER,
      GL_VERTEX_SHADER,
      GL_INFO_LOG_LENGTH);

   type Vertex_Attrib_Enum is
     (GL_VERTEX_ATTRIB_ARRAY_ENABLED,
      GL_VERTEX_ATTRIB_ARRAY_SIZE,
      GL_VERTEX_ATTRIB_ARRAY_STRIDE,
      GL_VERTEX_ATTRIB_ARRAY_TYPE,
      GL_CURRENT_VERTEX_ATTRIB,
      GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
      GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING);

   type Vertex_Attrib_Type is
     (GL_BYTE,
      GL_UNSIGNED_BYTE,
      GL_SHORT,
      GL_UNSIGNED_SHORT,
      GL_INT,
      GL_UNSIGNED_INT,
      GL_FLOAT,
      GL_FIXED);

   type Attribute_Type is
     (GL_FLOAT_VEC2,
      GL_FLOAT_VEC3,
      GL_FLOAT_VEC4,
      GL_INT_VEC2,
      GL_INT_VEC3,
      GL_INT_VEC4,
      GL_BOOL,
      GL_BOOL_VEC2,
      GL_BOOL_VEC3,
      GL_BOOL_VEC4,
      GL_FLOAT_MAT2,
      GL_FLOAT_MAT3,
      GL_FLOAT_MAT4,
      GL_SAMPLER_2D,
      GL_SAMPLER_CUBE);

   type Internal_Format_PName is
     (GL_SAMPLES);

   type Framebuffer_Attachment_Parameter_Name is
     (GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
      GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL,
      GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE);

   type Precision_Type is
     (GL_LOW_FLOAT,
      GL_MEDIUM_FLOAT,
      GL_HIGH_FLOAT,
      GL_LOW_INT,
      GL_MEDIUM_INT,
      GL_HIGH_INT);

   type Vertex_Attrib_Pointer_Type is
     (GL_BYTE,
      GL_UNSIGNED_BYTE,
      GL_SHORT,
      GL_UNSIGNED_SHORT,
      GL_INT,
      GL_UNSIGNED_INT,
      GL_FLOAT,
      GL_FIXED);

   type Get_Framebuffer_Parameter is
     (GL_SAMPLE_BUFFERS,
      GL_SAMPLES,
      GL_IMPLEMENTATION_COLOR_READ_TYPE,
      GL_IMPLEMENTATION_COLOR_READ_FORMAT);

   type Path_Gen_Mode is
     (GL_NONE);

   type Path_Transform_Type is
     (GL_NONE);

   type Path_Font_Style is
     (GL_NONE);

   type Program_Property_ARB is
     (GL_DELETE_STATUS,
      GL_LINK_STATUS,
      GL_VALIDATE_STATUS,
      GL_INFO_LOG_LENGTH,
      GL_ATTACHED_SHADERS,
      GL_ACTIVE_UNIFORMS,
      GL_ACTIVE_UNIFORM_MAX_LENGTH,
      GL_ACTIVE_ATTRIBUTES,
      GL_ACTIVE_ATTRIBUTE_MAX_LENGTH);

   type Context_WebGL_Type is new Context_Type with private;
   type Context_WebGL_Access is access all Context_WebGL_Type;
   type Pointer_To_Context_WebGL_Class is access all Context_WebGL_Type'Class;

   procedure Get_Drawing_Context_WebGL
     (Context   : in out Context_WebGL_Type;
      Canvas  : in out Canvas_Type'Class);

   procedure Begin_Render
     (Context : in out Context_WebGL_Type);

   procedure End_Render
     (Context : in out Context_WebGL_Type);

   procedure Perspective
     (Context       : in out Context_WebGL_Type'Class;
      Matrix        : out Matrix_4;
      Field_Of_View : GLfloat;
      Aspect_Ratio  : GLfloat;
      Near, Far     : GLfloat);

   procedure Translate
     (Context : in out Context_WebGL_Type'Class;
      Matrix  : in out Matrix_4;
      X, Y, Z : GLfloat);

   procedure Rotate
     (Context : in out Context_WebGL_Type'Class;
      Matrix  : in out Matrix_4;
      Angle   : GLfloat;
      X, Y, Z : GLfloat);

   procedure Uniform_Matrix
     (Context  : in out Context_WebGL_Type'Class;
      Location : GLuint;
      Matrix   : Matrix_4);

   procedure Active_Texture
     (Context : in out Context_WebGL_Type'Class;
      Texture : Texture_Unit);

   procedure Attach_Shader
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Shader : GLuint);

   procedure Bind_Attrib_Location
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Index : GLuint;
      Name : String);

   procedure Bind_Buffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Buffer : GLuint);

   procedure Bind_Framebuffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Framebuffer : GLuint);

   procedure Bind_Renderbuffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Renderbuffer_Target;
      Renderbuffer : GLuint);

   procedure Bind_Texture
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Texture : GLuint);

   procedure Blend_Color
     (Context : in out Context_WebGL_Type'Class;
      Red : GLfloat;
      Green : GLfloat;
      Blue : GLfloat;
      Alpha : GLfloat);

   procedure Blend_Equation
     (Context : in out Context_WebGL_Type'Class;
      Mode : Blend_Equation_Mode_EXT);

   procedure Blend_Equation_Separate
     (Context : in out Context_WebGL_Type'Class;
      ModeRGB : Blend_Equation_Mode_EXT;
      ModeAlpha : Blend_Equation_Mode_EXT);

   procedure Blend_Func
     (Context : in out Context_WebGL_Type'Class;
      Sfactor : Blending_Factor;
      Dfactor : Blending_Factor);

   procedure Blend_Func_Separate
     (Context : in out Context_WebGL_Type'Class;
      SfactorRGB : Blending_Factor;
      DfactorRGB : Blending_Factor;
      SfactorAlpha : Blending_Factor;
      DfactorAlpha : Blending_Factor);

   procedure Buffer_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Data : Float_Array;
      Usage : Buffer_Usage_ARB);

   procedure Buffer_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Data : Uint16_Array;
      Usage : Buffer_Usage_ARB);

   procedure Buffer_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Data : Uint8_Array;
      Usage : Buffer_Usage_ARB);

   procedure Buffer_Sub_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Offset : GLintptr;
      Data : Float_Array);

   procedure Buffer_Sub_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Offset : GLintptr;
      Data : Uint16_Array);

   procedure Buffer_Sub_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Offset : GLintptr;
      Data : Uint8_Array);

   procedure Clear
     (Context : in out Context_WebGL_Type'Class;
      Mask : Clear_Buffer_Mask_Array);

   procedure Clear_Color
     (Context : in out Context_WebGL_Type'Class;
      Red : GLfloat;
      Green : GLfloat;
      Blue : GLfloat;
      Alpha : GLfloat);

   procedure Clear_Depth
     (Context : in out Context_WebGL_Type'Class;
      D : GLfloat);

   procedure Clear_Stencil
     (Context : in out Context_WebGL_Type'Class;
      S : GLint);

   procedure Color_Mask
     (Context : in out Context_WebGL_Type'Class;
      Red : Boolean;
      Green : Boolean;
      Blue : Boolean;
      Alpha : Boolean);

   procedure Compile_Shader
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint);

   procedure Compressed_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      ImageSize : GLsizei;
      Data : Float_Array);

   procedure Compressed_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      ImageSize : GLsizei;
      Data : Uint16_Array);

   procedure Compressed_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      ImageSize : GLsizei;
      Data : Uint8_Array);

   procedure Compressed_Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      ImageSize : GLsizei;
      Data : Float_Array);

   procedure Compressed_Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      ImageSize : GLsizei;
      Data : Uint16_Array);

   procedure Compressed_Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      ImageSize : GLsizei;
      Data : Uint8_Array);

   procedure Copy_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint);

   procedure Copy_Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei);

   function Create_Program
     (Context : in out Context_WebGL_Type'Class)
     return GLuint;

   function Create_Shader
     (Context : in out Context_WebGL_Type'Class;
      Item_Type : Shader_Type)
     return GLuint;

   procedure Cull_Face
     (Context : in out Context_WebGL_Type'Class;
      Mode : Cull_Face_Mode);

   procedure Delete_Buffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Buffers : GLuint);

   procedure Delete_Framebuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Framebuffers : GLuint);

   procedure Delete_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint);

   procedure Delete_Renderbuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Renderbuffers : GLuint);

   procedure Delete_Shader
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint);

   procedure Delete_Textures
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Textures : GLuint);

   procedure Depth_Func
     (Context : in out Context_WebGL_Type'Class;
      Func : Depth_Function);

   procedure Depth_Mask
     (Context : in out Context_WebGL_Type'Class;
      Flag : Boolean);

   procedure Depth_Range
     (Context : in out Context_WebGL_Type'Class;
      N : GLfloat;
      F : GLfloat);

   procedure Detach_Shader
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Shader : GLuint);

   procedure Disable
     (Context : in out Context_WebGL_Type'Class;
      Cap : Enable_Cap);

   procedure Disable_Vertex_Attrib_Array
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint);

   procedure Draw_Arrays
     (Context : in out Context_WebGL_Type'Class;
      Mode : Primitive_Type;
      First : GLint;
      Count : GLsizei);

   procedure Draw_Elements
     (Context : in out Context_WebGL_Type'Class;
      Mode : Primitive_Type;
      Count : GLsizei;
      ElementType : Draw_Elements_Type;
      Offset : Natural);

   procedure Enable
     (Context : in out Context_WebGL_Type'Class;
      Cap : Enable_Cap);

   procedure Enable_Vertex_Attrib_Array
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint);

   procedure Finish
     (Context : in out Context_WebGL_Type'Class);

   procedure Flush
     (Context : in out Context_WebGL_Type'Class);

   procedure Framebuffer_Renderbuffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Attachment : Framebuffer_Attachment;
      Renderbuffertarget : Renderbuffer_Target;
      Renderbuffer : GLuint);

   procedure Framebuffer_Texture_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Attachment : Framebuffer_Attachment;
      Textarget : Texture_Target;
      Texture : GLuint;
      Level : GLint);

   procedure Front_Face
     (Context : in out Context_WebGL_Type'Class;
      Mode : Front_Face_Direction);

   procedure Gen_Buffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Buffers : GLuint);

   procedure Gen_Framebuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Framebuffers : GLuint);

   procedure Gen_Renderbuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Renderbuffers : GLuint);

   procedure Gen_Textures
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Textures : GLuint);

   procedure Generate_Mipmap
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target);

   procedure Get_Active_Attrib
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Index : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      Size : GLint;
      Item_Type : Attribute_Type;
      Name : String);

   procedure Get_Active_Uniform
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Index : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      Size : GLint;
      Item_Type : Attribute_Type;
      Name : String);

   procedure Get_Attached_Shaders
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      MaxCount : GLsizei;
      Count : GLsizei;
      Shaders : GLuint);

   function Get_Attrib_Location
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Name : String)
     return GLuint;

   procedure Get_Booleanv
     (Context : in out Context_WebGL_Type'Class;
      Pname : Get_PName;
      Data : Boolean);

   procedure Get_Buffer_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Pname : GLenum;
      Params : GLint);

   procedure Get_Floatv
     (Context : in out Context_WebGL_Type'Class;
      Pname : Get_PName;
      Data : GLfloat);

   procedure Get_Framebuffer_Attachment_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Attachment : Framebuffer_Attachment;
      Pname : Framebuffer_Attachment_Parameter_Name;
      Params : GLint);

   procedure Get_Integerv
     (Context : in out Context_WebGL_Type'Class;
      Pname : Get_PName;
      Data : GLint);

   procedure Get_Program_Info_Log
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      InfoLog : String);

   procedure Get_Programiv
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Pname : Program_Property_ARB;
      Params : GLint);

   procedure Get_Renderbuffer_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Renderbuffer_Target;
      Pname : Renderbuffer_Parameter_Name;
      Params : GLint);

   procedure Get_Shader_Info_Log
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      InfoLog : String);

   procedure Get_Shader_Precision_Format
     (Context : in out Context_WebGL_Type'Class;
      Shadertype : Shader_Type;
      Precisiontype : Precision_Type;
      Item_Range : GLint;
      Precision : GLint);

   procedure Get_Shader_Source
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      Source : String);

   procedure Get_Shaderiv
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      Pname : Shader_Parameter_Name;
      Params : GLint);

   procedure Get_Tex_Parameterfv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Get_Texture_Parameter;
      Params : GLfloat);

   procedure Get_Tex_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Get_Texture_Parameter;
      Params : GLint);

   function Get_Uniform_Location
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Name : String)
     return GLuint;

   procedure Get_Uniformfv
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Location : GLint;
      Params : GLfloat);

   procedure Get_Uniformiv
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Location : GLint;
      Params : GLint);

   procedure Get_Vertex_Attribfv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      Pname : GLenum;
      Params : GLfloat);

   procedure Get_Vertex_Attribiv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      Pname : GLenum;
      Params : GLint);

   procedure Hint
     (Context : in out Context_WebGL_Type'Class;
      Target : Hint_Target;
      Mode : Hint_Mode);

   function Is_Buffer
     (Context : in out Context_WebGL_Type'Class;
      Buffer : GLuint)
     return Boolean;

   function Is_Enabled
     (Context : in out Context_WebGL_Type'Class;
      Cap : Enable_Cap)
     return Boolean;

   function Is_Framebuffer
     (Context : in out Context_WebGL_Type'Class;
      Framebuffer : GLuint)
     return Boolean;

   function Is_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint)
     return Boolean;

   function Is_Renderbuffer
     (Context : in out Context_WebGL_Type'Class;
      Renderbuffer : GLuint)
     return Boolean;

   function Is_Shader
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint)
     return Boolean;

   function Is_Texture
     (Context : in out Context_WebGL_Type'Class;
      Texture : GLuint)
     return Boolean;

   procedure Line_Width
     (Context : in out Context_WebGL_Type'Class;
      Width : GLfloat);

   procedure Link_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint);

   procedure Pixel_Storei
     (Context : in out Context_WebGL_Type'Class;
      Pname : Pixel_Store_Parameter;
      Param : GLint);

   procedure Polygon_Offset
     (Context : in out Context_WebGL_Type'Class;
      Factor : GLfloat;
      Units : GLfloat);

   procedure Release_Shader_Compiler
     (Context : in out Context_WebGL_Type'Class);

   procedure Renderbuffer_Storage
     (Context : in out Context_WebGL_Type'Class;
      Target : Renderbuffer_Target;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei);

   procedure Sample_Coverage
     (Context : in out Context_WebGL_Type'Class;
      Value : GLfloat;
      Invert : Boolean);

   procedure Scissor
     (Context : in out Context_WebGL_Type'Class;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei);

   procedure Shader_Binary
     (Context : in out Context_WebGL_Type'Class;
      Count : GLsizei;
      Shaders : GLuint;
      Binaryformat : GLenum;
      Binary : Float_Array;
      Length : GLsizei);

   procedure Shader_Binary
     (Context : in out Context_WebGL_Type'Class;
      Count : GLsizei;
      Shaders : GLuint;
      Binaryformat : GLenum;
      Binary : Uint16_Array;
      Length : GLsizei);

   procedure Shader_Binary
     (Context : in out Context_WebGL_Type'Class;
      Count : GLsizei;
      Shaders : GLuint;
      Binaryformat : GLenum;
      Binary : Uint8_Array;
      Length : GLsizei);

   procedure Shader_Source
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      Source : String);

   procedure Stencil_Func
     (Context : in out Context_WebGL_Type'Class;
      Func : Stencil_Function;
      Ref : GLint;
      Mask : GLuint);

   procedure Stencil_Func_Separate
     (Context : in out Context_WebGL_Type'Class;
      Face : Stencil_Face_Direction;
      Func : Stencil_Function;
      Ref : GLint;
      Mask : GLuint);

   procedure Stencil_Mask
     (Context : in out Context_WebGL_Type'Class;
      Mask : GLuint);

   procedure Stencil_Mask_Separate
     (Context : in out Context_WebGL_Type'Class;
      Face : Stencil_Face_Direction;
      Mask : GLuint);

   procedure Set_Stencil_Op
     (Context : in out Context_WebGL_Type'Class;
      Fail : Stencil_Op;
      Zfail : Stencil_Op;
      Zpass : Stencil_Op);

   procedure Stencil_Op_Separate
     (Context : in out Context_WebGL_Type'Class;
      Face : Stencil_Face_Direction;
      Sfail : Stencil_Op;
      Dpfail : Stencil_Op;
      Dppass : Stencil_Op);

   procedure Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Float_Array);

   procedure Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Uint16_Array);

   procedure Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Uint8_Array);

   procedure Tex_Image_2D
     (Context        : in out Context_WebGL_Type'Class;
      Target         : Texture_Target;
      Level          : GLint;
      Internalformat : Internal_Format;
      Format         : Pixel_Format;
      Item_Type      : Pixel_Type;
      Image          : Gnoga.Gui.Element.Element_Type'Class);

   procedure Tex_Image_2D
     (Context        : in out Context_WebGL_Type'Class;
      Target         : Texture_Target;
      Level          : GLint;
      Internalformat : Internal_Format;
      Format         : Pixel_Format;
      Item_Type      : Pixel_Type;
      Image_Id       : String);

   procedure Tex_Parameter
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Param : GLfloat);

   procedure Tex_Parameterfv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Params : GLfloat);

   procedure Tex_Parameteri
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Param : GLint);

   procedure Tex_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Params : GLint);

   procedure Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Float_Array);

   procedure Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Uint16_Array);

   procedure Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Uint8_Array);

   procedure Uniform_1
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLfloat);

   procedure Uniform_1fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLfloat);

   procedure Uniform_1i
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint);

   procedure Uniform_1iv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLint);

   procedure Uniform_2
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLfloat;
      V1 : GLfloat);

   procedure Uniform_2fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLfloat);

   procedure Uniform_2i
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint;
      V1 : GLint);

   procedure Uniform_2iv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLint);

   procedure Uniform_3
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLfloat;
      V1 : GLfloat;
      V2 : GLfloat);

   procedure Uniform_3fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLfloat);

   procedure Uniform_3i
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint;
      V1 : GLint;
      V2 : GLint);

   procedure Uniform_3iv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLint);

   procedure Uniform_4
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLfloat;
      V1 : GLfloat;
      V2 : GLfloat;
      V3 : GLfloat);

   procedure Uniform_4fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLfloat);

   procedure Uniform_4i
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint;
      V1 : GLint;
      V2 : GLint;
      V3 : GLint);

   procedure Uniform_4iv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLint);

   procedure Uniform_Matrix_2fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Transpose : Boolean;
      Value : GLfloat);

   procedure Uniform_Matrix_3fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Transpose : Boolean;
      Value : GLfloat);

   procedure Uniform_Matrix_4fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Transpose : Boolean;
      Value : GLfloat);

   procedure Use_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint);

   procedure Validate_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint);

   procedure Vertex_Attrib_1
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat);

   procedure Vertex_Attrib_1fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat);

   procedure Vertex_Attrib_2
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat;
      Y : GLfloat);

   procedure Vertex_Attrib_2fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat);

   procedure Vertex_Attrib_3
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat;
      Y : GLfloat;
      Z : GLfloat);

   procedure Vertex_Attrib_3fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat);

   procedure Vertex_Attrib_4
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat;
      Y : GLfloat;
      Z : GLfloat;
      W : GLfloat);

   procedure Vertex_Attrib_4fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat);

   procedure Vertex_Attrib_Pointer
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      Size : GLuint;
      ItemType : Vertex_Attrib_Pointer_Type;
      Normalized : Boolean;
      Stride : GLsizei;
      Pointer : Natural);

   procedure Viewport
     (Context : in out Context_WebGL_Type'Class;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei);

   function Create_Buffer
     (Context : in out Context_WebGL_Type'Class)
     return GLuint;

   function Create_Texture
     (Context : in out Context_WebGL_Type'Class)
     return GLuint;

   function Get_Shader_Parameter
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      Parameter : Shader_Parameter_Name)
     return Boolean;

   function Get_Program_Parameter
     (Context : in out Context_WebGL_Type'Class;
      Progam : GLuint;
      Parameter : Program_Property_ARB)
     return Boolean;

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, String);

   type Context_WebGL_Type is new Context_Type with
      record
         Web_Object_Ids : String_Vectors.Vector;
         Render_Script  : String_Vectors.Vector;
         Rendering      : Boolean := False;
         View_Width     : Natural;
         View_Height    : Natural;
      end record;

end Gnoga.Gui.Element.Canvas.Context_WebGL;
