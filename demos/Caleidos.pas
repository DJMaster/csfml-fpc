program Caleidos;

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
    '#ifdef GL_ES'#10+
    'precision mediump float;'#10+
    '#endif'#10+
    ''#10+
    '#extension GL_OES_standard_derivatives : enable'#10+
    ''#10+
    'uniform float time;'#10+
    'uniform vec2 resolution;'#10+
    ''#10+
    'const float PI = 3.14159;'#10+
    ''#10+
    'vec3 hsv(float h, float s, float v) {'#10+
    '	float c = s * v;'#10+
    '	float _ = mod(h * 6.0, 6.0);'#10+
    '	vec3 C = vec3(c, c*(1.0 - abs(mod(_, 2.0) - 1.0)), 0.0);'#10+
    '	if (_ < 1.0) {'#10+
    '		C = vec3(C.x, C.y, C.z);'#10+
    '	} else if (_ < 2.0) {'#10+
    '		C = vec3(C.y, C.x, C.z);'#10+
    '	} else if (_ < 3.0) {'#10+
    '		C = vec3(C.z, C.x, C.y);'#10+
    '	} else if (_ < 4.0) {'#10+
    '		C = vec3(C.z, C.y, C.x);'#10+
    '	} else if (_ < 5.0) {'#10+
    '		C = vec3(C.y, C.z, C.x);'#10+
    '	} else {'#10+
    '		C = vec3(C.x, C.z, C.y);'#10+
    '	}'#10+
    '	return C + (v - c);'#10+
    '}'#10+
    ''#10+
    'float map(vec3 p) {'#10+
    '	return 2.0 - length(p.xz);'#10+
    '}'#10+
    ''#10+
    'float noise(vec2 co){'#10+
    '    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);'#10+
    '}'#10+
    ''#10+
    'void main( void ) {'#10+
    '	vec2 p = (2.0 * gl_FragCoord.xy - resolution) / resolution.y;'#10+
    '	vec3 cp = vec3(cos(time * 0.2), 0.0, sin(time * 0.45)) * 0.5;'#10+
    '	vec3 cl = vec3(-sin(time), 10.0, cos(time));'#10+
    '	vec3 cf = normalize(cl - cp);'#10+
    '	vec3 cs = normalize(cross(cf, vec3(sin(time * 0.1), 0.0, cos(time * 0.1))));'#10+
    '	vec3 cu = normalize(cross(cs, cf));'#10+
    '	float focus = 0.5;'#10+
    '	vec3 rd = normalize(cs * p.x + cu * p.y + cf * focus);'#10+
    '	vec3 rp = cp;'#10+
    '	for (int i = 0; i < 64; ++i) {'#10+
    '		float d = map(rp);'#10+
    '		if (d < 0.001)'#10+
    '			break;'#10+
    '		rp += rd * d;'#10+
    '	}'#10+
    '	float a = (atan(rp.z, rp.x)) * 16.0 / PI;'#10+
    '	float ai = floor(a);'#10+
    '	float af = fract(a);'#10+
    '	float d = (rp.y + 0.5 * time) * 10.0;'#10+
    '	float di = floor(d);'#10+
    '	float df = fract(d);'#10+
    '	float v = 32.0 * af * (1.0 - af) * df * (1.0 - df) * exp(-rp.y * 0.8);'#10+
    '	gl_FragColor = vec4(hsv(noise(vec2(ai, di) * 0.01), 1.0, v), 1.0);'#10+
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
