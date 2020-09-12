program vertexarraypoint;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CSFMLGraphics,
  CSFMLWindow,
  CSFMLConfig,
  CSFMLSystem;

const
  PARTICLE_LENGH = 1000000;
  Width = 800;
  Height = 600;

  procedure fillVertexArrayBuffer(var buffer: PsfVertexArray);
  var
    i: integer;
    vertex: sfVertex;
    position: sfVector2f;
  begin
    vertex := Default(sfVertex);
    position := Default(sfVector2f);
    vertex.position := position;
    vertex.color := sfWhite;
    for i := 0 to PARTICLE_LENGH - 1 do
    begin
      sfVertexArray_append(buffer, vertex);
    end;

  end;

  procedure updateVertexArrayBuffer(buffer: PsfVertexArray; target: PsfRenderWindow);
  var
    i: integer;
    vertex: PsfVertex;
  begin
    for i := 0 to PARTICLE_LENGH do
    begin
      vertex := sfVertexArray_getVertex(buffer, i);
      vertex^.color := sfColor_fromRGBA(Random($FF), Random($FF),
        Random($FF), Random($FF));
      vertex^.position.x := Random(Width);
      vertex^.position.y := Random(Height);
    end;
    sfRenderWindow_drawVertexArray(target, buffer, nil);
  end;

var
  mode: sfVideoMode;
  window: PsfRenderWindow;
  event: sfEvent;
  buffer: PsfVertexArray;

begin
  event := Default(sfEvent);
  mode.bitsPerPixel := 32;
  mode.Width := Width;
  mode.Height := Height;

  buffer := sfVertexArray_create;
  sfVertexArray_setPrimitiveType(buffer, sfPoints);
  fillVertexArrayBuffer(buffer);

  window := sfRenderWindow_create(mode, 'vertexarray - points', sfUint32(sfClose), nil);
  if not Assigned(window) then
    raise Exception.Create('Create window fail');
  sfRenderWindow_setFramerateLimit(window, 60);

  while sfRenderWindow_isOpen(window) = sfTrue do
  begin
    while sfRenderWindow_pollEvent(window, @event) = sfTrue do
    begin
      if event.type_ = sfEvtClosed then
        sfRenderWindow_close(window);
    end;

    sfRenderWindow_clear(window, sfBlack);
    updateVertexArrayBuffer(buffer, window);
    sfRenderWindow_display(window);
  end;
  sfRenderWindow_destroy(window);
  sfVertexArray_destroy(buffer);
end.

