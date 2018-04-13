program Colorful;

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
begin
{$ifdef LINUX}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

{$endif}
  States.BlendMode := sfBlendAlpha;
  States.Transform := sfTransform_identity;

  FragmentShader :=
    '// Colorful Voronoi'#10 +
    '// By: Brandon Fogerty'#10 +
    '// bfogerty at gmail dot com'#10 +
    '// xdpixel.com'#10 +
    ''#10 +
    '#ifdef GL_ES'#10 +
    'precision mediump float;'#10 +
    '#endif'#10 +
    ''#10 +
    'uniform float time;'#10 +
    'uniform vec2 resolution;'#10 +
    ''#10 +
    'vec2 hash(vec2 p)'#10 +
    '{'#10 +
    '    mat2 m = mat2(  13.85, 47.77,'#10 +
    '                    99.41, 88.48'#10 +
    '                );'#10 +
    ''#10 +
    '    return fract(tan(m*p*p*p*p*p*p) * 46727.29);'#10 +
    '}'#10 +
    ''#10 +
    'float voronoi(vec2 p)'#10 +
    '{'#10 +
    '    vec2 g = floor(p);'#10 +
    '    vec2 f = fract(p);'#10 +
    ''#10 +
    '    float distanceToClosestFeaturePoint = 1.0;'#10 +
    '    for(int y = -1; y <= 1; y++)'#10 +
    '    {'#10 +
    '        for(int x = -1; x <= 1; x++)'#10 +
    '        {'#10 +
    '            vec2 latticePoint = vec2(x, y);'#10 +
    '            float currentDistance = distance(latticePoint + hash(g+latticePoint), f);'#10 +
    '            distanceToClosestFeaturePoint = min(distanceToClosestFeaturePoint, currentDistance);'#10 +
    '        }'#10 +
    '    }'#10 +
    ''#10 +
    '    return distanceToClosestFeaturePoint;'#10 +
    '}'#10 +
    ''#10 +
    'void main( void )'#10 +
    '{'#10 +
    '    vec2 uv = ( gl_FragCoord.xy / resolution.xy ) * 2.0 - 1.0;'#10 +
    '    uv.x *= resolution.x / resolution.y;'#10 +
    ''#10 +
    '    float offset = voronoi(uv*10.0 + vec2(time));'#10 +
    '    float t = 1.0/abs(((uv.x + sin(uv.y + time)) + offset) * 30.0);'#10 +
    ''#10 +
    '    float r = voronoi( uv * 1.0 ) * 10.0;'#10 +
    '    vec3 finalColor = vec3(10.0 * uv.y, 2.0, 1.0 * r) * t;'#10 +
    ''#10 +
    '    gl_FragColor = vec4(finalColor, 1.0 );'#10 +
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

    sfShader_setFloatParameter(ShaderHandle, 'time', sfTime_asSeconds(sfClock_getElapsedTime(Clock)) );

    // Draw the sprite with the shader on it
    sfRenderWindow_drawSprite(WindowHandle, SpriteHandle, @States);

    // Finally, display the rendered frame on screen
    sfRenderWindow_display(WindowHandle);
  end;

  sfSprite_destroy(SpriteHandle);
  sfRenderWindow_Destroy(WindowHandle);
end.
