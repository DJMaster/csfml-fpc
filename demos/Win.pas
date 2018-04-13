program Win;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Windows,
  Messages,
  ctypes,
  CSFMLConfig,
  CSFMLAudio,
  CSFMLGraphics,
  CSFMLNetwork,
  CSFMLSystem,
  CSFMLWindow;

var
  Button: HWND;

function OnEvent(Handle: HWND; &Message: Cardinal; WParam: WPARAM;
  LParam: LPARAM): LRESULT; stdcall;
begin
  Result := 0;
  case (message) of
    // Quit when we close the main Window
    WM_CLOSE:
      begin
        PostQuitMessage(0);
        Exit;
      end;

    // Quit when we click the "quit" button
    WM_COMMAND:
      if HWND(lParam) = Button then
      begin
        PostQuitMessage(0);
        Exit;
      end;
  end;

  Result := DefWindowProc(Handle, Message, WParam, LParam);
end;

var
  WindowClass: WNDCLASS;
  Window: HWND;
  Title: string;
  View1, View2: HWND;
  SfmlView1, SfmlView2: PsfRenderWindow;
  Texture1, Texture2: PsfTexture;
  Sprite1, Sprite2: PsfSprite;
  Origin: sfVector2f;
  Position: sfVector2f;
  Clock: PsfClock;
  &Message: MSG;
  Time: Single;
begin
  // Define a class for our main Window
  WindowClass.style         := 0;
  WindowClass.lpfnWndProc   := @OnEvent;
  WindowClass.cbClsExtra    := 0;
  WindowClass.cbWndExtra    := 0;
  WindowClass.hInstance     := hInstance;
  WindowClass.hIcon         := 0;
  WindowClass.hCursor       := 0;
  WindowClass.hbrBackground := HBRUSH(COLOR_BACKGROUND);
  WindowClass.lpszMenuName  := nil;
  WindowClass.lpszClassName := 'SFML App';
  RegisterClass(WindowClass);

  // Let's create the main window
  Title := 'SFML Win - ' + lowerCase({$I %FPCTARGETCPU%}) + '-' + lowerCase({$I %FPCTARGETOS%});
  Window := CreateWindow('SFML App', PChar(Title), WS_SYSMENU or WS_VISIBLE, 200, 200, 660, 520, 0, 0, hInstance, nil);

  // Add a button for exiting
  Button := CreateWindow('BUTTON', 'Quit', WS_CHILD or WS_VISIBLE, 560, 440, 80, 40, Window, 0, hInstance, nil);

  // Let's create two SFML views
  View1 := CreateWindow('STATIC', nil, WS_CHILD or WS_VISIBLE or WS_CLIPSIBLINGS, 20,  20, 300, 400, Window, 0, hInstance, nil);
  View2 := CreateWindow('STATIC', nil, WS_CHILD or WS_VISIBLE or WS_CLIPSIBLINGS, 340, 20, 300, 400, Window, 0, hInstance, nil);
  SfmlView1 := sfRenderWindow_createFromHandle(pointer(View1), nil);
  SfmlView2 := sfRenderWindow_createFromHandle(pointer(View2), nil);

  // Load some textures to display
  Texture1 := sfTexture_createFromFile('../resources/image1.jpg', nil);
  Texture2 := sfTexture_createFromFile('../resources/image2.jpg', nil);
  Sprite1 := sfSprite_create();
  sfSprite_setTexture(Sprite1, Texture1, sfFalse);
  Sprite2 := sfSprite_create();
  sfSprite_setTexture(Sprite2, Texture2, sfFalse);

  Origin.x := sfTexture_getSize(Texture1).x;
  Origin.y := sfTexture_getSize(Texture1).y;
  sfSprite_setOrigin(Sprite1, Origin);
  Position := Origin;
  sfSprite_setPosition(Sprite1, Position);

  // Create a clock for measuring elapsed time
  Clock := sfClock_create();

  // Loop until a WM_QUIT message is received
  repeat
    if (PeekMessage(&Message, 0, 0, 0, PM_REMOVE)) then
    begin
      // If a message was waiting in the message queue, process it
      TranslateMessage(&Message);
      DispatchMessage(&Message);
    end
    else
    begin
      Time := sfTime_asSeconds(sfClock_getElapsedTime(Clock));

      // Clear views
      sfRenderWindow_clear(SfmlView1, sfBlack);
      sfRenderWindow_clear(SfmlView2, sfBlack);

      // Draw sprite 1 on view 1
      sfSprite_setRotation(Sprite1, Time * 100);
      sfRenderWindow_drawSprite(SfmlView1, Sprite1, nil);

      // Draw sprite 2 on view 2
      Position.x := Cos(Time) * 100;
      Position.y := 0;
      sfSprite_setPosition(Sprite2, Position);
      sfRenderWindow_drawSprite(SfmlView2, Sprite2, nil);

      // Display each view on screen
      sfRenderWindow_display(SfmlView1);
      sfRenderWindow_display(SfmlView2);
    end;
  until &Message.&Message = WM_QUIT;

  // Destroy the main Window (all its child controls will be destroyed)
  DestroyWindow(Window);

  // Don't forget to unregister the window class
  UnregisterClass('SFML App', hInstance);
end.
