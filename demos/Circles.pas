program Circles;

{$mode objfpc}{$H+}

uses
  SysUtils,
  ctypes,
  CSFMLConfig,
  CSFMLAudio,
  CSFMLGraphics,
  CSFMLNetwork,
  CSFMLSystem,
  CSFMLWindow;

var
  VertexShader, FragmentShader: PChar;
  RenderTargetHandle: PsfRenderTexture;
  TextureHandle: PsfTexture;
  Mode: sfVideoMode;
  WindowHandle: PsfRenderWindow;
  Title: string;
  Event: sfEvent;
  States: sfRenderStates;
  CircleShaderHandle: PsfShader;
  SpriteHandle: PsfSprite;
begin
  States.BlendMode := sfBlendAlpha;
  States.Transform := sfTransform_identity;

  VertexShader :=
    '#version 110'#10 +
    '#ifdef GL_ES'#10 +
    'precision lowp float;'#10 +
    '#endif'#10 +
    ''#10 +
    'varying vec2 uv;'#10 +
    ''#10 +
    'void'#10 +
    'main() {'#10 +
    '  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;'#10 +
    '  uv = vec2(gl_MultiTexCoord0);'#10 +
    '}';
  FragmentShader :=
    '#version 110'#10 +
    '#ifdef GL_ES'#10 +
    'precision lowp float;'#10 +
    '#endif'#10 +
    ''#10 +
    'varying vec2 uv;'#10 +
    'uniform float Radius;'#10 +
    'uniform vec2 Position;'#10 +
    'uniform vec4 CircleColor;'#10 +
    ''#10 +
    'void'#10 +
    'main() {'#10 +
    '  vec4 color0 = CircleColor;'#10 +
    ''#10 +
    '  vec2 m = uv - Position;'#10 +
    '  float dist = Radius - sqrt(m.x * m.x + m.y * m.y);'#10 +
    ''#10 +
    '  float t = 0.0;'#10 +
    '  if (dist > 2.0)'#10 +
    '    t = 1.0;'#10 +
    '  else if (dist > 0.0)'#10 +
    '    t = 0.5 * dist;'#10 +
    ''#10 +
    '  color0.a = t * color0.a;'#10 +
    '  gl_FragColor = color0;'#10 +
    '}';
  CircleShaderHandle := sfShader_createFromMemory(VertexShader, nil, FragmentShader);
  States.Shader := CircleShaderHandle;

  RenderTargetHandle := sfRenderTexture_create(800, 600, sfFalse);
  TextureHandle := sfRenderTexture_getTexture(RenderTargetHandle);

  SpriteHandle := sfSprite_create();
  sfSprite_setTexture(SpriteHandle, TextureHandle, sfFalse);

  // Create the main Window
  Mode.Width := 800;
  Mode.Height := 600;
  Mode.BitsPerPixel := 32;
  Title := 'SFML Shader - ' + lowerCase({$I %FPCTARGETCPU%}) + '-' + lowerCase({$I %FPCTARGETOS%});
  WindowHandle := sfRenderWindow_create(Mode, PChar(Title), sfUint32(sfTitleBar) or sfUint32(sfClose), nil);

  // Start the loop
  while (sfRenderWindow_isOpen(WindowHandle) = sfTrue) do
  begin
    // Process events
    while (sfRenderWindow_pollEvent(WindowHandle, @Event) = sfTrue) do
    begin
      if Event.type_ = sfEvtClosed then
        sfRenderWindow_close(WindowHandle);
    end;

    sfShader_setColorParameter(CircleShaderHandle, 'CircleColor', sfColor_fromRGBA(Random($FF), Random($FF), Random($FF), Random($FF)));
    sfShader_setFloatParameter(CircleShaderHandle, 'Radius', 200 * Random);
    sfShader_setFloat2Parameter(CircleShaderHandle, 'Position', 800 * Random, 600 * Random);

    sfRenderWindow_drawSprite(WindowHandle, SpriteHandle, @States);

    // Finally, display the rendered frame on screen
    sfRenderWindow_display(WindowHandle);
  end;

  sfSprite_destroy(SpriteHandle);
  sfRenderWindow_Destroy(WindowHandle);
end.
