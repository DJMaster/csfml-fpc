program HelloWorld;

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
  Mode: sfVideoMode;
  Window: PsfRenderWindow;
  Title: string;
  Texture: PsfTexture;
  Sprite: PsfSprite;
  Font: PsfFont;
  Text: PsfText;
  Music: PsfMusic;
  Event: sfEvent;
  TextPos: sfVector2f;
begin
{$ifdef LINUX}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

{$endif}
  Mode.Width := 800;
  Mode.Height := 600;
  Mode.BitsPerPixel := 32;

  Title := 'SFML Window - ' + lowerCase({$I %FPCTARGETCPU%}) + '-' + lowerCase({$I %FPCTARGETOS%});
  Window := sfRenderWindow_Create(Mode, PChar(Title), sfUint32(sfResize) or sfUint32(sfClose), nil);
  if not Assigned(Window) then
    raise Exception.Create('Window error');

  // Load a sprite to display
  Texture := sfTexture_CreateFromFile('../resources/oncapintada.jpg', nil);
  if not Assigned(Texture) then
    raise Exception.Create('Texture error');
  Sprite := sfSprite_Create();
  sfSprite_SetTexture(Sprite, Texture, sfTrue);

  // Create a graphical text to display
  Font := sfFont_createFromFile('../resources/admirationpains.ttf');
  if not Assigned(Font) then
    raise Exception.Create('Font error');
  Text := sfText_Create();
  
  sfText_SetString(Text, 'Hello World');
  sfText_SetFont(Text, Font);
  sfText_SetCharacterSize(Text, 50);
  sfText_SetColor(Text, sfBlack);
  TextPos.X := 300;
  TextPos.Y := 20;
  sfText_SetPosition(Text, TextPos);

  // Load a music to play
  Music := sfMusic_CreateFromFile('../resources/oncapintada.ogg');
  if not Assigned(Music) then
    raise Exception.Create('Music error');

  // Play the music
  sfMusic_Play(Music);

  // Start the game loop
  while (sfRenderWindow_IsOpen(Window) = sfTrue) do
  begin
    // Process events
    while (sfRenderWindow_PollEvent(Window, @Event) = sfTrue) do
    begin
      // Close window : exit
      if (Event.type_ = sfEvtClosed) then
        sfRenderWindow_Close(Window);
    end;

    // Clear the screen
    sfRenderWindow_clear(Window, sfWhite);

    // Draw the sprite
    sfRenderWindow_drawSprite(Window, Sprite, nil);

    // Draw the text
    sfRenderWindow_drawText(Window, Text, nil);

    // Update the window
    sfRenderWindow_display(Window);
  end;

  // Cleanup resources
  sfMusic_Destroy(Music);
  sfText_Destroy(Text);
  sfFont_Destroy(Font);
  sfSprite_Destroy(Sprite);
  sfTexture_Destroy(Texture);
  sfRenderWindow_Destroy(Window);
end.
