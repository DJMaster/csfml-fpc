program Dark;

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

  // Create the main Window
  Mode.width := 800;
  Mode.height := 600;
  Mode.bitsPerPixel := 32;
  Title := 'SFML Shader - ' + lowerCase({$I %FPCTARGETCPU%}) + '-' + lowerCase({$I %FPCTARGETOS%});
  WindowHandle := sfRenderWindow_create(Mode, PChar(Title), sfUint32(sfTitleBar) or sfUint32(sfClose), nil);
  sfRenderWindow_setMouseCursorVisible(WindowHandle, sfFalse); // hide the cursor

  RenderTargetHandle := sfRenderTexture_create(Mode.width, Mode.height, sfFalse);
  TextureHandle := sfRenderTexture_getTexture(RenderTargetHandle);

  SpriteHandle := sfSprite_create();
  sfSprite_setTexture(SpriteHandle, TextureHandle, sfFalse);

  FragmentShader :=
    '// made by darkstalker'#10 +
    '#ifdef GL_ES'#10 +
    'precision mediump float;'#10 +
    '#endif'#10 +
    ''#10 +
    'uniform float time;'#10 +
    'uniform vec2 mouse;'#10 +
    'uniform vec2 resolution;'#10 +
    ''#10 +
    '#define M_Pi 3.14159265358979'#10 +
    ''#10 +
    'mat2 calcRotationMat3(float ang)'#10 +
    '{'#10 +
    '	return mat2(cos(ang), -sin(ang), sin(ang), cos(ang));'#10 +
    '}'#10 +
    ''#10 +
    ''#10 +
    'void main(void)'#10 +
    '{'#10 +
    '	mat2 rotMatrix = calcRotationMat3(M_Pi*0.25*time*0.1);'#10 +
    ''#10 +
    '	vec2 screen_pos = gl_FragCoord.xy;'#10 +
    '	vec2 mouse_pos = mouse*resolution;'#10 +
    ''#10 +
    '	vec2 p = rotMatrix*screen_pos * 0.2;'#10 +
    '	float value = clamp((cos(p.x) + cos(p.y)) * 10., .1, 1.);'#10 +
    '	float light = 0.08 + clamp(1. - distance(screen_pos, mouse_pos) / 150., 0., 1.);'#10 +
    ''#10 +
    '	gl_FragColor = vec4(vec3(value*light), 1.);'#10 +
    '}';
  ShaderHandle := sfShader_createFromMemory(nil, nil, FragmentShader);
  States.Shader := ShaderHandle;

  sfShader_setFloat2Parameter(ShaderHandle, 'resolution', Mode.width, Mode.height);

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

    sfShader_setFloatParameter(ShaderHandle, 'time', sfTime_asSeconds(sfClock_getElapsedTime(Clock)) );

    MousePos := sfMouse_getPositionRenderWindow(WindowHandle);
    sfShader_setFloat2Parameter(ShaderHandle, 'mouse', MousePos.x / Mode.Width, 1 - MousePos.y / Mode.Height);

    // Draw the sprite with the shader on it
    sfRenderWindow_drawSprite(WindowHandle, SpriteHandle, @States);

    // Finally, display the rendered frame on screen
    sfRenderWindow_display(WindowHandle);
  end;
  
  sfSprite_destroy(SpriteHandle);
  sfRenderWindow_Destroy(WindowHandle);
end.
