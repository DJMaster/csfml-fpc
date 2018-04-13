program Sky;

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
    '#ifdef GL_ES'#10 +
    'precision mediump float;'#10 +
    '#endif'#10 +
    '///  Compack code for a 1K demo // Harley'#10 +
    'uniform float time;'#10 +
    'uniform vec2 resolution;'#10 +
    ''#10 +
    'mat2 m = mat2( 0.90,  0.110, -0.70,  1.00 );'#10 +
    ''#10 +
    'float ha( float n ) {return fract(sin(n)*758.5453);}'#10 +
    ''#10 +
    'float no( in vec3 x )'#10 +
    '{    vec3 p = floor(x);    vec3 f = fract(x);'#10 +
    '    float n = p.x + p.y*57.0 + p.z*800.0;float res = mix(mix(mix( ha(n+  0.0), ha(n+  1.0),f.x), mix( ha(n+ 57.0), ha(n+ 58.0),f.x),f.y),'#10 +
    '    mix(mix( ha(n+800.0), ha(n+801.0),f.x), mix( ha(n+857.0), ha(n+858.0),f.x),f.y),f.z);'#10 +
    '    return res;}'#10 +
    ''#10 +
    'float fbm( vec3 p )'#10 +
    '{    float f = 0.3*cos(time*0.03);'#10 +
    '    f += 0.50000*no( p ); p = p*2.02;    f -= 0.25000*no( p ); p = p*2.03;'#10 +
    '    f += 0.12500*no( p ); p = p*2.01;    f += 0.06250*no( p ); p = p*2.04;'#10 +
    '    f -= 0.03125*no( p );    return f/0.984375;}'#10 +
    ''#10 +
    'float cloud(vec3 p)'#10 +
    '{	p-=fbm(vec3(p.x,p.y,0.0)*0.5)*2.25;float a =0.0;	a-=fbm(p*3.0)*2.2-1.1;'#10 +
    '	if (a<0.0) a=0.0;a=a*a;	return a;}'#10 +
    ''#10 +
    'vec3 f2(vec3 c)'#10 +
    '{	c+=ha(gl_FragCoord.x+gl_FragCoord.y*.9)*0.01;'#10 +
    '	c*=0.7-length(gl_FragCoord.xy / resolution.xy -0.5)*0.7;'#10 +
    '	float w=length(c);'#10 +
    '	c=mix(c*vec3(1.0,1.0,1.6),vec3(w,w,w)*vec3(1.4,1.2,1.0),w*1.1-0.2);'#10 +
    '	return c;}'#10 +
    'void main( void ) {'#10 +
    '	vec2 position = ( gl_FragCoord.xy / resolution.xy ) ;'#10 +
    '	position.y+=0.2;	vec2 coord= vec2((position.x-0.5)/position.y,1.0/(position.y+0.2));'#10 +
    '	coord+=time*0.1;	float q = cloud(vec3(coord*1.0,0.222));'#10 +
    'vec3 	col =vec3(0.2,0.4,0.5) + vec3(q*vec3(0.2,0.4,0.1));'#10 +
    '	gl_FragColor = vec4( f2(col), 1.0 );'#10 +
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
