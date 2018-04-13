program Circle;

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
const
  Radius = 200;

var
  VideoMode: sfVideoMode;
  RenderWindow: PsfRenderWindow;
  Title: string;
  Event: sfEvent;
  CircleShape: PsfCircleShape;
  Origin: sfVector2f;
begin
{$ifdef LINUX}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

{$endif}
  // Create the main Window
  VideoMode.Width := 2 * Radius;
  VideoMode.Height := 2 * Radius;
  VideoMode.BitsPerPixel := 32;
  Title := 'SFML Circle - ' + lowerCase({$I %FPCTARGETCPU%}) + '-' + lowerCase({$I %FPCTARGETOS%});
  RenderWindow := sfRenderWindow_create(VideoMode, PChar(Title), sfUint32(sfTitleBar) or sfUint32(sfResize) or sfUint32(sfClose), nil);

  CircleShape := sfCircleShape_create();
  Origin.X := Radius;
  Origin.Y := Radius;
  sfCircleShape_setOrigin(CircleShape, Origin);
  sfCircleShape_setRadius(CircleShape, Radius);
  sfCircleShape_setOutlineThickness(CircleShape, Radius);
  sfCircleShape_setOutlineColor(CircleShape, sfWhite);
  sfCircleShape_setFillColor(CircleShape, sfGreen);

  // Start the loop
  while (sfRenderWindow_isOpen(RenderWindow) = sfTrue) do
  begin
    // Process events
    while (sfRenderWindow_pollEvent(RenderWindow, @Event) = sfTrue) do
    begin
      if Event.type_ = sfEvtClosed then
        sfRenderWindow_close(RenderWindow);
    end;

    // Clear the screen
    sfRenderWindow_clear(RenderWindow, sfRed);

    // Draw the circle
    sfRenderWindow_drawCircleShape(RenderWindow, CircleShape, nil);

    // Finally, display the rendered frame on screen
    sfRenderWindow_display(RenderWindow);
  end;

  // Cleanup resources
  sfCircleShape_destroy(CircleShape);
  sfRenderWindow_Destroy(RenderWindow);
end.
