program Tunnel;

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
    '// Endless Tunnel'#10 +
    '// By: Brandon Fogerty'#10 +
    '// bfogerty at gmail dot com'#10 +
    ''#10 +
    '#ifdef GL_ES'#10 +
    'precision mediump float;'#10 +
    '#endif'#10 +
    ''#10 +
    'uniform float time;'#10 +
    'uniform vec2 mouse;'#10 +
    'uniform vec2 resolution;'#10 +
    ''#10 +
    '#define HorizontalAmplitude      0.80'#10 +
    '#define VerticleAmplitude        0.80'#10 +
    '#define HorizontalSpeed          0.90'#10 +
    '#define VerticleSpeed            0.50'#10 +
    '#define ParticleMinSize          1.76'#10 +
    '#define ParticleMaxSize          1.71'#10 +
    '#define ParticleBreathingSpeed   0.30'#10 +
    '#define ParticleColorChangeSpeed 0.70'#10 +
    '#define ParticleCount            7.0'#10 +
    '#define ParticleColor1           vec3(9.0, 5.0, 3.0)'#10 +
    '#define ParticleColor2           vec3(1.0, 3.0, 9.0)'#10 +
    ''#10 +
    ''#10 +
    'vec3 checkerBoard( vec2 uv, vec2 pp )'#10 +
    '{'#10 +
    '    vec2 p = floor( uv * 4.6 );'#10 +
    '    float t = mod( p.x + p.y, 2.2);'#10 +
    '    vec3 c = vec3(t+pp.x, t+pp.y, t+(pp.x*pp.y));'#10 +
    ''#10 +
    '    return c;'#10 +
    '}'#10 +
    ''#10 +
    'vec3 tunnel( vec2 p, float scrollSpeed, float rotateSpeed )'#10 +
    '{'#10 +
    '    float a = 2.0 * atan( p.x, p.y  );'#10 +
    '    float po = 2.0;'#10 +
    '    float px = pow( p.x*p.x, po );'#10 +
    '    float py = pow( p.y*p.y, po );'#10 +
    '    float r = pow( px + py, 1.0/(2.0*po) );'#10 +
    '    vec2 uvp = vec2( 1.0/r + (time*scrollSpeed), a + (time*rotateSpeed));'#10 +
    '    vec3 finalColor = checkerBoard( uvp, p ).xyz;'#10 +
    '    finalColor *= r;'#10 +
    ''#10 +
    '    return finalColor;'#10 +
    '}'#10 +
    ''#10 +
    'vec3 particles( vec2 uv )'#10 +
    '{'#10 +
    '	vec2 pos = uv * 2.0 - 1.0;'#10 +
    '	pos.x *= (resolution.x / resolution.y);'#10 +
    ''#10 +
    '	vec3 c = vec3( 0, 0, 0 );'#10 +
    ''#10 +
    '	for( float i = 1.0; i < ParticleCount+1.0; ++i )'#10 +
    '	{'#10 +
    '		float cs = cos( time * HorizontalSpeed * (i/ParticleCount) ) * HorizontalAmplitude;'#10 +
    '		float ss = sin( time * VerticleSpeed   * (i/ParticleCount) ) * VerticleAmplitude;'#10 +
    '		vec2 origin = vec2( cs , ss );'#10 +
    ''#10 +
    '		float t = sin( time * ParticleBreathingSpeed * i ) * 0.5 + 0.5;'#10 +
    '		float particleSize = mix( ParticleMinSize, ParticleMaxSize, t );'#10 +
    '		float d = clamp( sin( length( pos - origin )  + particleSize ), 0.0, particleSize);'#10 +
    ''#10 +
    '		float t2 = sin( time * ParticleColorChangeSpeed * i ) * 0.5 + 0.5;'#10 +
    '		vec3 color = mix( ParticleColor1, ParticleColor2, t2 );'#10 +
    '		c += color * pow( d, 70.0 );'#10 +
    '	}'#10 +
    ''#10 +
    '	return c;'#10 +
    '}'#10 +
    ''#10 +
    'void main(void)'#10 +
    '{'#10 +
    '    vec2 uv = gl_FragCoord.xy / resolution.xy;'#10 +
    '    float timeSpeedX = time * 0.3;'#10 +
    '    float timeSpeedY = time * 0.2;'#10 +
    '    vec2 p = uv + vec2( -0.50+cos(timeSpeedX)*0.2, -0.5-sin(timeSpeedY)*0.3 );'#10 +
    ''#10 +
    '    vec3 finalColor = tunnel( p , 1.0, 0.0);'#10 +
    ''#10 +
    ''#10 +
    '    timeSpeedX = time * 0.30001;'#10 +
    '    timeSpeedY = time * 0.20001;'#10 +
    '    p = uv + vec2( -0.50+cos(timeSpeedX)*0.2, -0.5-sin(timeSpeedY)*0.3 );'#10 +
    ''#10 +
    ''#10 +
    '	   finalColor += particles( uv );'#10 +
    ''#10 +
    '    gl_FragColor = vec4( finalColor, 1.0 );'#10 +
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
