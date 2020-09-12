## csfml-fpc

[SFML/CSFML](http://www.sfml-dev.org/) headers binding for the Free Pascal Compiler.

* **Basic example**
```pascal
program basic;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CSFMLGraphics,
  CSFMLWindow,
  CSFMLConfig,
  CSFMLSystem { you can add units after this };

var
  mode: sfVideoMode;
  event: sfEvent;
  window: PsfRenderWindow;
begin
  event := Default(sfEvent);
  mode.bitsPerPixel := 32;
  mode.Width := 800;
  mode.Height := 600;

  window := sfRenderWindow_create(mode, 'Basic Window', sfUint32(sfClose), nil);
  sfRenderWindow_setFramerateLimit(window, 60);

  if not Assigned(window) then
    raise Exception.Create('Create window fail');

  while sfRenderWindow_isOpen(window) = sfTrue do
  begin
    while sfRenderWindow_pollEvent(window, @event) = sfTrue do
    begin
      if event.type_ = sfEvtClosed then
        sfRenderWindow_close(window);
    end;

    sfRenderWindow_clear(window, sfBlue);
    sfRenderWindow_display(window);

  end;

  sfRenderWindow_destroy(window);
end.
```

* **Examples**

A few examples that demonstrates basic sfml constructs.



* [**Basic**](demos/basic.pas) - Just a basic window with only windows manager functions.

* [**Shape Circles**](demos/shcircles.pas) - Exemplifies how to draw circle shapes on screen.
* [**Text**](demos/text) - Draw a simple text on screen;
* [**Vertex Array - points**](demos/vertexarraypoint.pas) - Draws 1M points in screen using sfVertexArray.

