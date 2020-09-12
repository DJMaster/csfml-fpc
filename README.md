## csfml-fpc

[SFML/CSFML](http://www.sfml-dev.org/) headers binding for the Free Pascal Compiler.

* Basic example
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

| [Basic Window](demos/basic.pas)                        | [Shape Circles](demos/shcircles.pas)                |
| ------------------------------------------------------ | --------------------------------------------------- |
| <img src="images\basicwindow.PNG" style="zoom:33%;" /> | <img src="images\shcircles.PNG" style="zoom:33%;"/> |
| [**Basic Text**](demos/text.pas)                           |                                                     |
| <img src="images\text.png" style="zoom:33%;" />        |                                                     |

