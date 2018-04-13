program Pong;

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

// Define some constants
const
  Pi = 3.14159;
  GameWidth = 800;
  GameHeight = 600;
  PaddleSize: sfVector2f = (X: 25; Y: 100);
  BallRadius: cfloat = 10;
  AITime : sfTime = (MicroSeconds: 1000);
  PaddleSpeed: cfloat = 400;
  BallSpeed: cfloat = 400;
var
  Mode: sfVideoMode;
  Window: PsfRenderWindow;
  Title: string;
  BallSoundBuffer: PsfSoundBuffer;
  BallSound: PsfSound;
  Size: sfVector2f;
  Origin: sfVector2f;
  LeftPaddle, RightPaddle: PsfRectangleShape;
  Ball: PsfCircleShape;
  Font: PsfFont;
  Position: sfVector2f;
  LeftPosition, RightPosition, BallPosition: sfVector2f;
  PauseMessage: PsfText;
  AITimer, Clock: PsfClock;
  IsPlaying: cbool;
  Event: sfEvent;
  DeltaTime, Factor: cfloat;
  RightPaddleSpeed: cfloat = 0;
  BallAngle: cfloat = 0;
  Offset: sfVector2f;
begin
{$ifdef LINUX}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

{$endif}
  Randomize;

  Mode.width := GameWidth;
  Mode.height := GameHeight;
  Mode.bitsPerPixel := 32;

  // Create the window of the application
  Title := 'SFML Pong - ' + lowerCase({$I %FPCTARGETCPU%}) + '-' + lowerCase({$I %FPCTARGETOS%});
  Window := sfRenderWindow_create(Mode, PChar(Title), sfUint32(sfTitleBar) or sfUint32(sfClose), nil);
  sfRenderWindow_setVerticalSyncEnabled(Window, sfTrue);

  // Load the sounds used in the game
  BallSoundBuffer := sfSoundBuffer_createFromFile('../resources/ball.wav');
  BallSound := sfSound_create();
  sfSound_setBuffer(BallSound, BallSoundBuffer);

  // Create the left paddle
  LeftPaddle := sfRectangleShape_create();
  Size.x := PaddleSize.X - 3;
  Size.y := PaddleSize.Y - 3;
  sfRectangleShape_setSize(LeftPaddle, Size);
  sfRectangleShape_setOutlineThickness(LeftPaddle, 3);
  sfRectangleShape_setOutlineColor(LeftPaddle, sfBlack);
  sfRectangleShape_setFillColor(LeftPaddle, sfColor_fromRGB(100, 100, 200));
  Origin.x := 0.5 * PaddleSize.X;
  Origin.y := 0.5 * PaddleSize.Y;
  sfRectangleShape_setOrigin(LeftPaddle, Origin);

  // Create the right paddle
  RightPaddle := sfRectangleShape_create();
  Size.x := PaddleSize.X - 3;
  Size.y := PaddleSize.Y - 3;
  sfRectangleShape_setSize(RightPaddle, Size);
  sfRectangleShape_setOutlineThickness(RightPaddle, 3);
  sfRectangleShape_setOutlineColor(RightPaddle, sfBlack);
  sfRectangleShape_setFillColor(RightPaddle, sfColor_fromRGB(200, 100, 100));
  Origin.x := 0.5 * PaddleSize.X;
  Origin.y := 0.5 * PaddleSize.Y;
  sfRectangleShape_setOrigin(RightPaddle, Origin);

  // Create the ball
  Ball := sfCircleShape_create();
  sfCircleShape_setRadius(Ball, BallRadius - 3);
  sfCircleShape_setOutlineThickness(Ball, 3);
  sfCircleShape_setOutlineColor(Ball, sfBlack);
  sfCircleShape_setFillColor(Ball, sfWhite);
  Origin.x := 0.5 * BallRadius;
  Origin.y := 0.5 * BallRadius;
  sfCircleShape_setOrigin(Ball, Origin);

  // Load the text font
  Font := sfFont_createFromFile('../resources/sansation.ttf');

  // Initialize the pause message
  PauseMessage := sfText_create();
  sfText_setFont(PauseMessage, Font);
  sfText_setCharacterSize(PauseMessage, 40);
  Position.x := 170;
  Position.y := 150;
  sfText_setPosition(PauseMessage, Position);
  sfText_setColor(PauseMessage, sfWhite);
  sfText_setString(PauseMessage, 'Welcome to SFML pong!'#10'Press space to start the game');

  // Define the paddles properties
  AITimer := sfClock_create();
  Clock := sfClock_create();

  IsPlaying := False;
  while sfRenderWindow_isOpen(Window) = sfTrue do
  begin
    // Handle events
    while (sfRenderWindow_pollEvent(Window, @Event) = sfTrue) do
    begin
      // Window closed or escape key pressed: exit
      if (Event.type_ = sfEvtClosed) or
        ((Event.type_ = sfEvtKeyPressed) and (Event.key.Code = sfKeyEscape)) then
      begin
        sfRenderWindow_close(Window);
        Break;
      end;

      // Space key pressed: play
      if (Event.type_ = sfEvtKeyPressed) and (Event.key.Code = sfKeySpace) then
      begin
        if not IsPlaying then
        begin
          // (re)start the game
          IsPlaying := True;
          sfClock_restart(Clock);

          // Reset the position of the paddles and Ball
          LeftPosition.x := 10 + 0.5 * PaddleSize.X;
          LeftPosition.y := 0.5 * GameHeight;
          sfRectangleShape_setPosition(LeftPaddle, LeftPosition);
          
          RightPosition.x := GameWidth - 10 - 0.5 * PaddleSize.X;
          RightPosition.y := 0.5 * GameHeight;
          sfRectangleShape_setPosition(RightPaddle, RightPosition);
          
          BallPosition.x := 0.5 * GameWidth;
          BallPosition.y := 0.5 * GameHeight;
          sfCircleShape_setPosition(Ball, BallPosition);

          // Reset the Ball angle
          repeat
            // Make sure the ball initial angle is not too much vertical
            BallAngle := Random * 2 * Pi;
          until Abs(Cos(BallAngle)) >= 0.7;
        end;
      end;
    end;

    if IsPlaying then
    begin
      DeltaTime := sfTime_asSeconds(sfClock_restart(Clock));

      // Move the player's paddle
      LeftPosition := sfRectangleShape_getPosition(LeftPaddle);
      if (sfKeyboard_isKeyPressed(sfKeyUp) = sfTrue) and
        (LeftPosition.Y - 0.5 * PaddleSize.Y > 5) then
      begin
        Offset.x := 0;
        Offset.y := -PaddleSpeed * DeltaTime;
        sfRectangleShape_move(LeftPaddle, Offset);
      end;
      
      if (sfKeyboard_isKeyPressed(sfKeyDown) = sfTrue) and
        (LeftPosition.Y + 0.5 * PaddleSize.Y < GameHeight - 5) then
      begin
        Offset.x := 0;
        Offset.y := PaddleSpeed * DeltaTime;
        sfRectangleShape_move(LeftPaddle, Offset);
      end;

      // Move the computer's paddle
      RightPosition := sfRectangleShape_getPosition(RightPaddle);
      if ((RightPaddleSpeed < 0) and (RightPosition.Y - 0.5 * PaddleSize.Y > 5)) or
        ((RightPaddleSpeed > 0) and (RightPosition.Y + 0.5 * PaddleSize.Y < GameHeight - 5)) then
      begin
        Offset.x := 0;
        Offset.y := RightPaddleSpeed * DeltaTime;
        sfRectangleShape_move(RightPaddle, Offset);
      end;

      // Update the computer's paddle direction according to the ball position
      if sfClock_getElapsedTime(AITimer).MicroSeconds > AITime.MicroSeconds then
      begin
        sfClock_restart(AITimer);
        BallPosition := sfCircleShape_getPosition(Ball);
        if (BallPosition.Y + BallRadius > RightPosition.Y + 0.5 * PaddleSize.Y) then
          RightPaddleSpeed := PaddleSpeed
        else if (BallPosition.Y - BallRadius < RightPosition.Y - 0.5 * PaddleSize.Y) then
          RightPaddleSpeed := -PaddleSpeed
        else
          RightPaddleSpeed := 0;
      end;

      // Move the ball
      Factor := BallSpeed * DeltaTime;
      Offset.x := Cos(BallAngle) * Factor;
      Offset.y := Sin(BallAngle) * Factor;
      sfCircleShape_move(Ball, Offset);

      // Check collisions between the ball and the screen
      BallPosition := sfCircleShape_getPosition(Ball);
      if BallPosition.X - BallRadius < 0 then
      begin
        IsPlaying := False;
        sfText_setString(PauseMessage, 'You lost!'#10'Press space to restart or'#10'escape to exit');
      end
      else if BallPosition.X + BallRadius > GameWidth then
      begin
        IsPlaying := False;
        sfText_setString(PauseMessage, 'You won!'#10'Press space to restart or'#10'escape to exit');
      end;

      if (BallPosition.Y - BallRadius < 0) then
      begin
        sfSound_play(BallSound);
        BallAngle := -BallAngle;
        BallPosition.y := BallRadius + 0.1;
        sfCircleShape_setPosition(Ball, BallPosition);
      end
      else if (BallPosition.Y + BallRadius > GameHeight) then
      begin
        sfSound_play(BallSound);
        BallAngle := -BallAngle;
        BallPosition.y := GameHeight - BallRadius - 0.1;
        sfCircleShape_setPosition(Ball, BallPosition);
      end;

      // Check the collisions between the Ball and the paddles
      // Left Paddle
      BallPosition := sfCircleShape_getPosition(Ball);
      LeftPosition := sfRectangleShape_getPosition(LeftPaddle);
      if (BallPosition.X - BallRadius <= LeftPosition.X + 0.5 * PaddleSize.X) and
        (BallPosition.X - BallRadius >= LeftPosition.X) and
        (BallPosition.Y + BallRadius >= LeftPosition.Y - 0.5 * PaddleSize.Y) and
        (BallPosition.Y - BallRadius <= LeftPosition.Y + 0.5 * PaddleSize.Y) then
      begin
        if (BallPosition.Y > LeftPosition.Y) then
          BallAngle := Pi - BallAngle + Random * Pi / 9
        else
          BallAngle := Pi - BallAngle - Random * Pi / 9;

        sfSound_play(BallSound);
        BallPosition.x := LeftPosition.X + BallRadius + 0.5 * PaddleSize.X + 0.1;
        sfCircleShape_setPosition(Ball, BallPosition);
      end;

      // Right Paddle
      BallPosition := sfCircleShape_getPosition(Ball);
      RightPosition := sfRectangleShape_getPosition(RightPaddle);
      if (BallPosition.X + BallRadius >= RightPosition.X - 0.5 * PaddleSize.X) and
        (BallPosition.X + BallRadius <= RightPosition.X) and
        (BallPosition.Y + BallRadius >= RightPosition.Y - 0.5 * PaddleSize.Y) and
        (BallPosition.Y - BallRadius <= RightPosition.Y + 0.5 * PaddleSize.Y) then
      begin
        if (BallPosition.Y > RightPosition.Y) then
          BallAngle := Pi - BallAngle + Random * Pi / 9
        else
          BallAngle := Pi - BallAngle - Random * Pi / 9;

        sfSound_play(BallSound);
        BallPosition.x := RightPosition.X - BallRadius - 0.5 * PaddleSize.X - 0.1;
        sfCircleShape_setPosition(Ball, BallPosition);
      end;
    end; // isPlaying

    // Clear the window
    sfRenderWindow_clear(Window, sfColor_fromRGB(50, 200, 50));

    if IsPlaying then
    begin
      // Draw the paddles and the Ball
      sfRenderWindow_drawRectangleShape(Window, LeftPaddle, nil);
      sfRenderWindow_drawRectangleShape(Window, RightPaddle, nil);
      sfRenderWindow_drawCircleShape(Window, Ball, nil);
    end
    else
    begin
      // Draw the pause message
      sfRenderWindow_drawText(Window, PauseMessage, nil);
    end;

    // Display things on screen
    sfRenderWindow_display(Window);
  end;

  sfSound_destroy(BallSound);
  sfSoundBuffer_destroy(BallSoundBuffer);
  sfRenderWindow_Destroy(Window);
end.
