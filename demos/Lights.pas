program Lights;

{$mode objfpc}{$H+}

uses
  SysUtils,
{$ifdef LINUX}
  Math,
{$endif}
  ctypes,
  CSFMLConfig,
  CSFMLAudio,
  CSFMLGraphics,
  CSFMLNetwork,
  CSFMLSystem,
  CSFMLWindow;

var
  FragmentShader: PChar;
  RenderTargetHandle: PsfRenderTexture;
  TextureHandle: PsfTexture;
  Mode: sfVideoMode;
  WindowHandle: PsfRenderWindow;
  Title: string;
  Event: sfEvent;
  States: sfRenderStates;
  ShaderHandle: PsfShader;
  SpriteHandle: PsfSprite;
  Clock: PsfClock;
  MousePos: sfVector2i; 
begin
{$ifdef LINUX}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

{$endif}
  States.BlendMode := sfBlendAlpha;
  States.Transform := sfTransform_identity;

  FragmentShader :=
    '#ifdef GL_ES'#10 +
    'precision mediump float;'#10 +
    '#endif'#10 +
    ''#10 +
    'uniform float time;'#10 +
    'uniform vec2 mouse;'#10 +
    'uniform vec2 resolution;'#10 +
    ''#10 +
    'vec2 position;'#10 +
    ''#10 +
    'vec3 ball(vec3 colour, float sizec, float xc, float yc){'#10 +
    '	return colour * (sizec / distance(position, vec2(xc, yc)));'#10 +
    '}'#10 +
    ''#10 +
    'vec3 red = vec3(2, 1, 1);'#10 +
    'vec3 green = vec3(1, 2, 1);'#10 +
    'vec3 blue = vec3(2, 2, 3);'#10 +
    'void main( void ) {'#10 +
    ''#10 +
    '	position = ( gl_FragCoord.xy / resolution.xy );'#10 +
    '	position.y = position.y * resolution.y/resolution.x + 0.25;'#10 +
    '	vec2 mousepos = mouse;'#10 +
    '	mousepos.y = mouse.y * resolution.y/resolution.x + 0.25;'#10 +
    ''#10 +
    '	vec3 color = vec3(0.0);'#10 +
    '	float ratio = resolution.x / resolution.y;'#10 +
    '	color += ball(red, 0.01, sin(time*4.0) / 12.0 + 0.5, cos(time*4.0) / 6.0 + 0.5);'#10 +
    '	color += ball(green, 0.01, sin(time*4.0) / 6.0 + 0.5, cos(time*4.0) / 12.0 + 0.5);'#10 +
    '	color += ball(blue, 0.01, mousepos.x, mousepos.y);'#10 +
    '	gl_FragColor = vec4(color, 1.0 );'#10 +
    ''#10 +
    '}';
  ShaderHandle := sfShader_createFromMemory(nil, nil, FragmentShader);
  States.Shader := ShaderHandle;

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

  sfShader_setFloat2Parameter(ShaderHandle, 'resolution', Mode.Width, Mode.Height);

  Clock := sfClock_create();
  sfClock_restart(Clock);
  
  // Start the loop
  while (sfRenderWindow_isOpen(WindowHandle) = sfTrue) do
  begin
    // Process events
    while (sfRenderWindow_pollEvent(WindowHandle, @Event) = sfTrue) do
    begin
      if Event.type_ = sfEvtClosed then
        sfRenderWindow_close(WindowHandle);
    end;

    MousePos := sfMouse_getPositionRenderWindow(WindowHandle);
    sfShader_setFloat2Parameter(ShaderHandle, 'mouse', MousePos.x / Mode.Width, 1 - MousePos.y / Mode.Height);

    sfShader_setFloatParameter(ShaderHandle, 'time', sfTime_asSeconds(sfClock_getElapsedTime(Clock)) );

    // Draw the sprite with the shader on it
    sfRenderWindow_drawSprite(WindowHandle, SpriteHandle, @States);

    // Finally, display the rendered frame on screen
    sfRenderWindow_display(WindowHandle);
  end;

  sfSprite_destroy(SpriteHandle);
  sfRenderWindow_Destroy(WindowHandle);
end.
