program Shader;

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

type
  TEffect = class(TObject)
  private
    FName: string;
    FIsLoaded: Boolean;
    FFont: PsfFont;
  protected
    function OnLoad: Boolean; virtual; abstract;
    procedure OnUpdate(Time, X, Y: Single); virtual; abstract;
    procedure OnDraw(Target: PsfRenderWindow; States: sfRenderStates); virtual; abstract;
  public
    constructor Create(Name: string);
    procedure Load;

    procedure Update(Time, X, Y: Single);
    procedure Draw(Target: PsfRenderWindow; States: sfRenderStates);

    property Font: PsfFont read FFont write FFont;
    property Name: string read FName;
  end;

{ TEffect }

constructor TEffect.Create(Name: string);
begin
  FName := Name;
end;

procedure TEffect.Draw(Target: PsfRenderWindow; States: sfRenderStates);
var
  Error: PsfText;
  Position: sfVector2f;
begin
  if FIsLoaded then
    OnDraw(Target, States)
  else
  begin
    Error := sfText_create();
    sfText_setString(Error, 'Shader not supported');
    sfText_setFont(Error, FFont);
    Position.x := 220;
    Position.y := 200;
    sfText_setPosition(Error, Position);
    sfText_setCharacterSize(Error, 36);
    sfRenderWindow_drawText(Target, Error, @States);
  end;
end;

procedure TEffect.Load;
begin
  FIsLoaded := (sfShader_isAvailable() = sfTrue) and OnLoad;
end;

procedure TEffect.Update(Time, X, Y: Single);
begin
  if FIsLoaded then
    OnUpdate(Time, x, y);
end;

type
  TPixelate = class(TEffect)
  private
    FTexture: PsfTexture;
    FSprite: PsfSprite;
    FShader: PsfShader;
  public
    constructor Create;
    function OnLoad: Boolean; override;
    procedure OnUpdate(Time, X, Y: Single); override;
    procedure OnDraw(Target: PsfRenderWindow; States: sfRenderStates); override;
  end;

  TWaveBlur = class(TEffect)
  private
    FText: PsfText;
    FShader: PsfShader;
  public
    constructor Create;
    function OnLoad: Boolean; override;
    procedure OnUpdate(Time, X, Y: Single); override;
    procedure OnDraw(Target: PsfRenderWindow; States: sfRenderStates); override;
  end;

  TStormBlink = class(TEffect)
  private
    FPoints: PsfVertexArray;
    FShader: PsfShader;
  public
    constructor Create;
    function OnLoad: Boolean; override;
    procedure OnUpdate(Time, X, Y: Single); override;
    procedure OnDraw(Target: PsfRenderWindow; States: sfRenderStates); override;
  end;

  TEdge = class(TEffect)
  private
    FSurface: PsfRenderTexture;
    FBackgroundTexture: PsfTexture;
    FEntityTexture: PsfTexture;
    FBackgroundSprite: PsfSprite;
    FEntities: array of PsfSprite;
    FShader: PsfShader;
  public
    constructor Create;
    function OnLoad: Boolean; override;
    procedure OnUpdate(Time, X, Y: Single); override;
    procedure OnDraw(Target: PsfRenderWindow; States: sfRenderStates); override;
  end;

  TGeometry = class(TEffect)
  private
    FTexture: PsfTexture;
    FTransform: sfTransform;
    FPointCloud: PsfVertexArray; 
    FShader: PsfShader;
  public
    constructor Create;
    function OnLoad: Boolean; override;
    procedure OnUpdate(Time, X, Y: Single); override;
    procedure OnDraw(Target: PsfRenderWindow; States: sfRenderStates); override;
  end;


{ TPixelate }

constructor TPixelate.Create;
begin
  inherited Create('Pixelate');
  FSprite := sfSprite_create();
end;

procedure TPixelate.OnDraw(Target: PsfRenderWindow; States: sfRenderStates);
begin
  States.Shader := FShader;
  sfRenderWindow_drawSprite(Target, FSprite, @States);
end;

function TPixelate.OnLoad: Boolean;
begin
  // Load the texture and initialize the sprite
  Assert(FileExists('../resources/ladybug.jpg'));
  FTexture := sfTexture_createFromFile('../resources/ladybug.jpg', nil);
  sfSprite_setTexture(FSprite, FTexture, sfFalse);

  // Load the Shader
  Assert(FileExists('../resources/pixelate.frag'));
  FShader := sfShader_createFromFile(nil, nil, '../resources/pixelate.frag');

  Result := Assigned(FShader);
end;

procedure TPixelate.OnUpdate(Time, X, Y: Single);
begin
  sfShader_setFloatParameter(FShader, 'pixel_threshold', (X + Y) / 30);
end;


{ TWaveBlur }

constructor TWaveBlur.Create;
begin
  inherited Create('Wave + Blur');
  FText := sfText_create();
end;

procedure TWaveBlur.OnDraw(Target: PsfRenderWindow; States: sfRenderStates);
begin
  States.Shader := FShader;
  sfRenderWindow_drawText(Target, FText, @States);
end;

function TWaveBlur.OnLoad: Boolean;
var
  Position: sfVector2f;
begin
  // Create the text
  sfText_setString(FText, 'Praesent suscipit augue in velit pulvinar hendrerit varius purus aliquam.'#10 +
    'Mauris mi odio, bibendum quis fringilla a, laoreet vel orci. Proin vitae vulputate tortor.'#10 +
    'Praesent cursus ultrices justo, ut feugiat ante vehicula quis.'#10 +
    'Donec fringilla scelerisque mauris et viverra.'#10 +
    'Maecenas adipiscing ornare scelerisque. Nullam at libero elit.'#10 +
    'Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas.'#10 +
    'Nullam leo urna, tincidunt id semper eget, ultricies sed mi.'#10 +
    'Morbi mauris massa, commodo id dignissim vel, lobortis et elit.'#10 +
    'Fusce vel libero sed neque scelerisque venenatis.'#10 +
    'Integer mattis tincidunt quam vitae iaculis.'#10 +
    'Vivamus fringilla sem non velit venenatis fermentum.'#10 +
    'Vivamus varius tincidunt nisi id vehicula.'#10 +
    'Integer ullamcorper, enim vitae euismod rutrum, massa nisl semper ipsum,'#10 +
    'vestibulum sodales sem ante in massa.'#10 +
    'Vestibulum in augue non felis convallis viverra.'#10 +
    'Mauris ultricies dolor sed massa convallis sed aliquet augue fringilla.'#10 +
    'Duis erat eros, porta in accumsan in, blandit quis sem.'#10 +
    'In hac habitasse platea dictumst. Etiam fringilla est id odio dapibus sit amet semper dui laoreet.'#10);
  sfText_setFont(FText, FFont);
  sfText_setCharacterSize(FText, 22);
  Position.x := 30;
  Position.y := 20;
  sfText_setPosition(FText, Position);

  // Load the Shader
  Assert(FileExists('../resources/wave.vert'));
  Assert(FileExists('../resources/blur.frag'));
  FShader := sfShader_createFromFile('../resources/wave.vert', nil, '../resources/blur.frag');

  Result := Assigned(FShader);
end;

procedure TWaveBlur.OnUpdate(Time, X, Y: Single);
begin
  sfShader_setFloatParameter(FShader, 'wave_phase', Time);
  sfShader_setFloat2Parameter(FShader, 'wave_amplitude', X * 40, Y * 40);
  sfShader_setFloatParameter(FShader, 'blur_radius', (X + Y) * 0.008);
end;


{ TStormBlink }

constructor TStormBlink.Create;
begin
  inherited Create('Storm + Blink');
  FPoints := sfVertexArray_create();
end;

procedure TStormBlink.OnDraw(Target: PsfRenderWindow; States: sfRenderStates);
begin
  States.Shader := FShader;
  sfRenderWindow_drawVertexArray(Target, FPoints, @States);
end;

function TStormBlink.OnLoad: Boolean;
var
  Index: Integer;
  Vertex: sfVertex;
begin
  // Create the points
  sfVertexArray_setPrimitiveType(FPoints, sfPoints);
  for Index := 1 to 40000 do
  begin
    Vertex.Position.x := 800 * Random;
    Vertex.Position.y := 600 * Random;
    Vertex.Color.R := Random(256);
    Vertex.Color.G := Random(256);
    Vertex.Color.B := Random(256);
    sfVertexArray_append(FPoints, Vertex);
  end;

  // Load the Shader
  Assert(FileExists('../resources/storm.vert'));
  Assert(FileExists('../resources/blink.frag'));
  FShader := sfShader_createFromFile('../resources/storm.vert', nil, '../resources/blink.frag');

  Result := Assigned(FShader);
end;

procedure TStormBlink.OnUpdate(Time, X, Y: Single);
var
  Radius: Single;
begin
  Radius := 200 + Cos(Time) * 150;
  sfShader_setFloat2Parameter(FShader, 'storm_position', X * 800, Y * 600);
  sfShader_setFloatParameter(FShader, 'storm_inner_radius', Radius / 3);
  sfShader_setFloatParameter(FShader, 'storm_total_radius', Radius);
  sfShader_setFloatParameter(FShader, 'blink_alpha', 0.5 + Cos(Time * 3) * 0.25);
end;


{ TEdge }

constructor TEdge.Create;
begin
  inherited Create('Edge Post-Effect');
  FBackgroundSprite := sfSprite_create();
end;

procedure TEdge.OnDraw(Target: PsfRenderWindow; States: sfRenderStates);
var
  Sprite: PsfSprite;
begin
  States.Shader := FShader;
  Sprite := sfSprite_create();
  sfSprite_setTexture(Sprite, sfRenderTexture_getTexture(FSurface), sfFalse);
  try
    sfRenderWindow_drawSprite(Target, Sprite, @States);
  finally
    sfSprite_destroy(Sprite);
  end;
end;

function TEdge.OnLoad: Boolean;
var
  Index: Integer;
  Entity: PsfSprite;
  Position: sfVector2f;
  IntRect: sfIntRect;
begin
  // Create the off-screen surface
  FSurface := sfRenderTexture_create(800, 600, sfFalse);
  sfRenderTexture_setSmooth(FSurface, sfTrue);

  // Load the textures
  Assert(FileExists('../resources/sfml.png'));
  FBackgroundTexture := sfTexture_createFromFile('../resources/sfml.png', nil);
  sfTexture_setSmooth(FBackgroundTexture, sfTrue);
  Assert(FileExists('../resources/devices.png'));
  FEntityTexture := sfTexture_createFromFile('../resources/devices.png', nil);
  sfTexture_setSmooth(FEntityTexture, sfTrue);

  // Initialize the background sprite
  sfSprite_setTexture(FBackgroundSprite, FBackgroundTexture, sfFalse);
  Position.x := 135;
  Position.y := 100;
  sfSprite_setPosition(FBackgroundSprite, Position);

  // Load the moving entities
  for Index := 0 to 5 do
  begin
    IntRect.left := 96 * Index;
    IntRect.top := 0;
    IntRect.width := 96;
    IntRect.height := 96;
    Entity := sfSprite_create();
    sfSprite_setTexture(Entity, FEntityTexture, sfFalse);
    sfSprite_setTextureRect(Entity, IntRect);
    SetLength(FEntities, Length(FEntities) + 1);
    FEntities[Length(FEntities) - 1] := Entity;
  end;

  // Load the Shader
  Assert(FileExists('../resources/edge.frag'));
  FShader := sfShader_createFromFile(nil, nil, '../resources/edge.frag');

  Result := Assigned(FShader);
end;

procedure TEdge.OnUpdate(Time, X, Y: Single);
var
  Index: Integer;
  Position: sfVector2f;
begin
  sfShader_setFloatParameter(FShader, 'edge_threshold', 1 - (x + y) / 2);

  // Update the position of the moving entities
  for Index := 0 to Length(FEntities) - 1 do
  begin
    Position.x := Cos(0.25 * (time * Index + (Length(FEntities) - Index))) * 300 + 350;
    Position.y := Sin(0.25 * (time * (Length(FEntities) - Index) + Index)) * 200 + 250;
    sfSprite_setPosition(FEntities[Index], Position);
  end;

  // Render the updated scene to the off-screen surface
  sfRenderTexture_clear(FSurface, sfWhite);
  sfRenderTexture_drawSprite(FSurface, FBackgroundSprite, nil);
  for Index := 0 to Length(FEntities) - 1 do
    sfRenderTexture_drawSprite(FSurface, FEntities[Index], nil);
  sfRenderTexture_display(FSurface);
end;


{ TGeometry }

constructor TGeometry.Create;
begin
  inherited Create('Geometry Shader Billboards');
  FPointCloud := sfVertexArray_create();
end;

procedure TGeometry.OnDraw(Target: PsfRenderWindow; States: sfRenderStates);
begin
  States.shader := FShader;
  States.texture := FTexture;
  States.transform := FTransform;

  sfRenderWindow_drawVertexArray(Target, FPointCloud, @States);
end;

function TGeometry.OnLoad: Boolean;
var
  Index: Integer;
  Vertex: sfVertex;
begin
  // Check if geometry shaders are supported
  if (sfShader_isGeometryAvailable() = sfFalse) then
  begin
    Result := false;
    exit;
  end;

  // Create the points
  for Index := 1 to 10000 do
  begin
    Vertex.Position.x := Random * 960 - 480;
    Vertex.Position.y := Random * 960 - 480;
    sfVertexArray_append(FPointCloud, Vertex);
  end;

  // Load the textures
  Assert(FileExists('../resources/logo.png'));
  FTexture := sfTexture_createFromFile('../resources/logo.png', nil);

  // Load the Shader
  Assert(FileExists('../resources/billboard.vert'));
  Assert(FileExists('../resources/billboard.geom'));
  Assert(FileExists('../resources/billboard.frag'));
  FShader := sfShader_createFromFile('../resources/billboard.vert', '../resources/billboard.geom', '../resources/billboard.frag');
  
  sfShader_setFloat2Parameter(FShader, 'resolution', 800, 600);

  Result := Assigned(FShader);
end;

procedure TGeometry.OnUpdate(Time, X, Y: Single);
var
  Size: Single;
begin
  // Reset our transformation matrix
  FTransform := sfTransform_identity;
  // Move to the center of the window
  sfTransform_translate(@FTransform, 400, 300);
  // Rotate everything based on cursor position
  sfTransform_rotate(@FTransform, x * 360);

  // Adjust billboard size to scale between 25 and 75
  Size := 25 + abs(y) * 50;

  // Update the shader parameter
  sfShader_setFloat2Parameter(FShader, 'size', Size, Size);
end;


var
  Window: PsfRenderWindow;
  Title: string;
  VideoMode: sfVideoMode;
  Font: PsfFont;
  Position: sfVector2f;
  MousePos: sfVector2i;
  Size: sfVector2u;
  Description, Instructions: PsfText;
  TextBackgroundTexture: PsfTexture;
  TextBackground: PsfSprite;
  State: sfRenderStates;
  Clock: PsfClock;
  Current, Index: Integer;
  Effects: array of TEffect;
  Event: sfEvent;
  X, Y: Single;
  Caption: string;
begin
{$ifdef LINUX}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

{$endif}
  State.BlendMode := sfBlendAlpha;
  State.Transform := sfTransform_identity;

  // Create the main Window
  VideoMode.width := 800;
  VideoMode.height := 600;
  VideoMode.BitsPerPixel := 32;
  Title := 'SFML Shader - ' + lowerCase({$I %FPCTARGETCPU%}) + '-' + lowerCase({$I %FPCTARGETOS%});
  Window := sfRenderWindow_create(VideoMode, PChar(Title), sfUint32(sfTitleBar) or sfUint32(sfClose), nil);
  sfRenderWindow_setVerticalSyncEnabled(Window, sfTrue);

  // Load the application font and pass it to the Effect class
  Font := sfFont_createFromFile('../resources/sansation.ttf');

  // Create the effects
  SetLength(Effects, 5);
  Effects[0] := TPixelate.Create;
  Effects[1] := TWaveBlur.Create;
  Effects[2] := TStormBlink.Create;
  Effects[3] := TEdge.Create;
  Effects[4] := TGeometry.Create;
  Current := 0;

  // Initialize them
  for Index := Low(Effects) to High(Effects) do
  begin
    Effects[Index].Font := Font;
    Effects[Index].Load;
  end;

  // Create the messages background
  Assert(FileExists('../resources/text-background.png'));
  TextBackgroundTexture := sfTexture_createFromFile('../resources/text-background.png', nil);

  TextBackground := sfSprite_create();
  sfSprite_setTexture(TextBackground, TextBackgroundTexture, sfFalse);
  Position.x := 0;
  Position.y := 520;
  sfSprite_setPosition(TextBackground, Position);
  sfSprite_setColor(TextBackground, sfColor_fromRGBA(255, 255, 255, 200));

  // Create the description text
  Description := sfText_create();
  Caption := 'Current effect: ' + Effects[Current].Name;

  sfText_SetString(Description, PChar(Caption));
  sfText_SetFont(Description, Font);
  sfText_SetCharacterSize(Description, 20);
  sfText_SetColor(Description, sfColor_fromRGB(80, 80, 80));
  Position.x := 10;
  Position.y := 530;
  sfText_SetPosition(Description, Position);

  // Create the instructions text
  Instructions := sfText_create();
  sfText_SetString(Instructions, 'Press left and right arrows to change the current shader');
  sfText_SetFont(Instructions, Font);
  sfText_SetCharacterSize(Instructions, 20);
  sfText_SetColor(Instructions, sfColor_fromRGB(80, 80, 80));
  Position.x := 280;
  Position.y := 555;
  sfText_SetPosition(Instructions, Position);

  // Start the game loop
  Clock := sfClock_create();
  while (sfRenderWindow_IsOpen(Window) = sfTrue) do
  begin
    // Process events
    while (sfRenderWindow_PollEvent(Window, @Event) = sfTrue) do
    begin
      // Close Window: exit
      if Event.type_ = sfEvtClosed then
        sfRenderWindow_Close(Window);

      if Event.type_ = sfEvtKeyPressed then
      begin
        case Event.Key.Code of
          // Escape key: exit
          sfKeyEscape:
            sfRenderWindow_Close(Window);

          // Left arrow key: previous Shader
          sfKeyLeft:
            begin
              if Current = 0 then
                Current := Length(Effects) - 1
              else
                Dec(Current);
              Caption := 'Current effect: ' + Effects[Current].Name;
              sfText_SetString(Description, PChar(Caption));
            end;

          // Right arrow key: next Shader
          sfKeyRight:
            begin
              if Current = High(Effects) then
                Current := 0
              else
                Inc(Current);
              Caption := 'Current effect: ' + Effects[Current].Name;
              sfText_SetString(Description, PChar(Caption));
            end;
        end;
      end;
    end;

    // Update the current example
    MousePos := sfMouse_getPositionRenderWindow(Window);
    Size := sfRenderWindow_getSize(Window);
    X := MousePos.x / Size.x;
    Y := MousePos.y / Size.y;
    Effects[Current].Update(sfTime_asSeconds(sfClock_getElapsedTime(Clock)), X, Y);

    // Clear the Window
    sfRenderWindow_clear(Window, sfColor_fromRGB(255, 128, 0));

    // Draw the current example
    Effects[Current].Draw(Window, State);

    // Draw the text
    sfRenderWindow_drawSprite(Window, TextBackground, nil);
    sfRenderWindow_drawText(Window, Instructions, nil);
    sfRenderWindow_drawText(Window, Description, nil);

    // Finally, display the rendered frame on screen
    sfRenderWindow_display(Window);
  end;

  // delete the effects
  for Index := Low(Effects) to High(Effects) do
    Effects[Index].Free;

  sfFont_destroy(Font);
  sfRenderWindow_Destroy(Window);
end.
