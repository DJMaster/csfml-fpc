program Fire;

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
    '#ifdef GL_ES'#10 +
    'precision mediump float;'#10 +
    '#endif'#10 +
    ''#10 +
    'uniform float time;'#10 +
    'uniform vec2 mouse;'#10 +
    'uniform vec2 resolution;'#10 +
    ''#10 +
    'float snoise(vec3 uv, float res)'#10 +
    '{'#10 +
    '  const vec3 s = vec3(1e0, 1e2, 1e3);'#10 +
    ''#10 +
    '  uv *= res;'#10 +
    ''#10 +
    '  vec3 uv0 = floor(mod(uv, res))*s;'#10 +
    '  vec3 uv1 = floor(mod(uv+vec3(1.), res))*s;'#10 +
    ''#10 +
    '  vec3 f = fract(uv); f = f*f*(3.0-2.0*f);'#10 +
    ''#10 +
    '  vec4 v = vec4(uv0.x+uv0.y+uv0.z, uv1.x+uv0.y+uv0.z,'#10 +
    '           uv0.x+uv1.y+uv0.z, uv1.x+uv1.y+uv0.z);'#10 +
    ''#10 +
    '  vec4 r = fract(sin(v*1e-1)*1e3);'#10 +
    '  float r0 = mix(mix(r.x, r.y, f.x), mix(r.z, r.w, f.x), f.y);'#10 +
    ''#10 +
    '  r = fract(sin((v + uv1.z - uv0.z)*1e-1)*1e3);'#10 +
    '  float r1 = mix(mix(r.x, r.y, f.x), mix(r.z, r.w, f.x), f.y);'#10 +
    '  return mix(r0, r1, f.z)*2.-1.;'#10 +
    '}'#10 +
    ''#10 +
    'void main( void ) {'#10 +
    '  vec2 p = gl_FragCoord.xy / resolution.xy;'#10 +
    '  p.x = p.x - mouse.x / resolution.x;'#10 +
    '  p.y = p.y + mouse.y / resolution.y;'#10 +
    '  p.y = p.y - 1.0;'#10 +
    ''#10 +
    '  p.x*=resolution.x/resolution.y;'#10 +
    ''#10 +
    '  float color = 3.0 - (6.*length(p));'#10 +
    ''#10 +
    '  vec3 coord = vec3(atan(p.x,p.y)/6.2832, length(p)*0.4, .5);'#10 +
    ''#10 +
    '  for(int i = 1; i <= 7; i++){'#10 +
    '    float power = pow(2.0, float(i));'#10 +
    '    color += (1.5 / power) * snoise(coord + vec3(0.,-time*.05, time*.01), power*16.);'#10 +
    '  }'#10 +
    ''#10 +
    '  gl_FragColor = vec4( color, pow(max(color,0.),2.)*0.4, pow(max(color,0.),3.)*0.15 , 1.0);'#10 +
    ''#10 +
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
    sfShader_setFloat2Parameter(ShaderHandle, 'mouse', MousePos.x, MousePos.y);

    // Draw the sprite with the shader on it
    sfRenderWindow_drawSprite(WindowHandle, SpriteHandle, @States);

    // Finally, display the rendered frame on screen
    sfRenderWindow_display(WindowHandle);
  end;
  
  sfSprite_destroy(SpriteHandle);
  sfRenderWindow_Destroy(WindowHandle);
end.
