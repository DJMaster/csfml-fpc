program shcircles;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CSFMLGraphics,
  CSFMLWindow,
  CSFMLConfig,
  CSFMLSystem { you can add units after this };

  procedure createCircleBuffer(var buffer: array of PsfCircleShape);
  var
    i: integer;
  begin
    for i := 1 to High(buffer) do
    begin
      buffer[i] := sfCircleShape_create();
    end;
  end;

  procedure updateCircleBuffer(window: PsfRenderWindow;
  var buffer: array of PsfCircleShape);
  var
    i: integer;
    position: sfVector2f;
  begin
    for i := 1 to High(buffer) do
    begin
      position.x := Random(800) - 40;
      position.y := Random(600) - 40;
      sfCircleShape_setPosition(buffer[i], position);
      sfCircleShape_setRadius(buffer[i], 100 * Random);
      sfCircleShape_setFillColor(buffer[i], sfColor_fromRGBA(Random($FF),
        Random($FF), Random($FF), Random($FF)));
      sfRenderWindow_drawCircleShape(window, buffer[i], nil);
    end;
  end;

  procedure destroyCircleBuffer(var buffer: array of PsfCircleShape);
  var
    i: integer;
  begin
    for i := 1 to High(buffer) do
    begin
      sfCircleShape_destroy(buffer[i]);
    end;
  end;

var
  mode: sfVideoMode;
  event: sfEvent;
  window: PsfRenderWindow;
  buffer: array [1 .. 2000] of PsfCircleShape;
begin
  event := Default(sfEvent);

  createCircleBuffer(buffer);

  mode.Width := 800;
  mode.Height := 600;
  mode.bitsPerPixel := 32;

  window := sfRenderWindow_create(mode, 'Shapes - Circles', sfUint32(sfClose), nil);
  sfWindow_setFramerateLimit(PsfWindow(window), 60);
  if not Assigned(window) then
    raise Exception.Create('cant create window');

  while sfRenderWindow_isOpen(window) = sfTrue do
  begin
    while sfRenderWindow_pollEvent(window, @event) = sfTrue do
    begin
      if event.type_ = sfEvtClosed then
        sfRenderWindow_close(window);
    end;

    sfRenderWindow_clear(window, sfBlue);
    updateCircleBuffer(window, buffer);
    sfRenderWindow_display(window);
  end;

  destroyCircleBuffer(buffer);
  sfRenderWindow_destroy(window);

end.
