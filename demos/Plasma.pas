	program Plasma;

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
    '#extension GL_OES_standard_derivatives : enable'#10 +
    ''#10 +
    'uniform float time;'#10 +
    'uniform vec2 mouse;'#10 +
    'uniform vec2 resolution;'#10 +
    ''#10 +
    'void main( void ) {'#10 +
    ''#10 +
    '	vec2 position = ( gl_FragCoord.xy / resolution.xy ) + mouse / 4.0;'#10 +
    ''#10 +
    '	float color = 0.0;'#10 +
    '	color += sin( position.x * cos( time / 15.0 ) * 4.0 ) + cos( position.y * cos( time / 15.0 ) * 10.0 );'#10 +
    '	color += sin( position.y * sin( time / 10.0 ) * 40.0 ) + cos( position.x * sin( time / 25.0 ) * 40.0 );'#10 +
    '	color += sin( position.x * sin( time / 5.0 ) * 10.0 ) + sin( position.y * sin( time / 35.0 ) * 80.0 );'#10 +
    '	color *= sin( time / 10.0 ) * 0.5;'#10 +
    ''#10 +
    '	gl_FragColor = vec4( vec3( color, color * 0.8, sin( color + time / 3.0 ) * 0.75 ), 1.0 );'#10 +
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
