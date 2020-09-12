program text;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  ctypes,
  CSFMLConfig,
  CSFMLAudio,
  CSFMLGraphics,
  CSFMLNetwork,
  CSFMLSystem,
  CSFMLWindow;

var
  event: sfEvent;
  mode: sfVideoMode;
  window: PsfRenderWindow;
  font: PsfFont;
  mtext: PsfText;
  textPosition: sfVector2f;
begin
  event := Default(sfEvent);
  mode.bitsPerPixel := 32;
  mode.Width := 800;
  mode.Height := 600;

  textPosition.x := 50;
  textPosition.y := 50;

  font := sfFont_createFromFile('../resources/Amadeus.ttf');
  if not Assigned(font) then
    raise Exception.Create('Font create fail');

  mtext := sfText_create();
  if not Assigned(mtext) then
    raise Exception.Create('Text create fail');

  sfText_setString(mtext,
    'The quick brown fox jumps over'#10'the lazy dog.'#10#10'Welcome to FPC CSFML.');
  sfText_setFont(mtext, font);
  sfText_setColor(mtext, sfWhite);
  sfText_setCharacterSize(mtext, 50);
  sfText_setPosition(mtext, textPosition);

  window := sfRenderWindow_create(mode, 'Simple sfml text use',
    sfUint32(sfClose), nil);

  if not Assigned(window) then
    raise Exception.Create('Window constructor fail');

  sfRenderWindow_setFramerateLimit(window, 30);

  while sfRenderWindow_isOpen(window) = sfTrue do
  begin
    while sfRenderWindow_pollEvent(window, @event) = sfTrue do
    begin
      if event.type_ = sfEvtClosed then
        sfRenderWindow_close(window);
    end;

    sfRenderWindow_clear(window, sfBlue);
    sfRenderWindow_drawText(window, mtext, nil);
    sfRenderWindow_display(window);
  end;
  sfRenderWindow_destroy(window);
end.



