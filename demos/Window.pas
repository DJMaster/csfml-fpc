program Window_;

{$mode objfpc}{$H+}

uses
  SysUtils,
{$ifdef LINUX}
  Math,
{$endif}
  ctypes,
  gl,
  CSFMLConfig,
  CSFMLAudio,
  CSFMLGraphics,
  CSFMLNetwork,
  CSFMLSystem,
  CSFMLWindow;

const
  Cube: array [0.. 251] of GLfloat = (
    // positions    // colors (r, g, b, a)
    -50, -50, -50,  0, 0, 1, 1,
    -50,  50, -50,  0, 0, 1, 1,
    -50, -50,  50,  0, 0, 1, 1,
    -50, -50,  50,  0, 0, 1, 1,
    -50,  50, -50,  0, 0, 1, 1,
    -50,  50,  50,  0, 0, 1, 1,

     50, -50, -50,  0, 1, 0, 1,
     50,  50, -50,  0, 1, 0, 1,
     50, -50,  50,  0, 1, 0, 1,
     50, -50,  50,  0, 1, 0, 1,
     50,  50, -50,  0, 1, 0, 1,
     50,  50,  50,  0, 1, 0, 1,

    -50, -50, -50,  1, 0, 0, 1,
     50, -50, -50,  1, 0, 0, 1,
    -50, -50,  50,  1, 0, 0, 1,
    -50, -50,  50,  1, 0, 0, 1,
     50, -50, -50,  1, 0, 0, 1,
     50, -50,  50,  1, 0, 0, 1,

    -50,  50, -50,  0, 1, 1, 1,
     50,  50, -50,  0, 1, 1, 1,
    -50,  50,  50,  0, 1, 1, 1,
    -50,  50,  50,  0, 1, 1, 1,
     50,  50, -50,  0, 1, 1, 1,
     50,  50,  50,  0, 1, 1, 1,

    -50, -50, -50,  1, 0, 1, 1,
     50, -50, -50,  1, 0, 1, 1,
    -50,  50, -50,  1, 0, 1, 1,
    -50,  50, -50,  1, 0, 1, 1,
     50, -50, -50,  1, 0, 1, 1,
     50,  50, -50,  1, 0, 1, 1,

    -50, -50,  50,  1, 1, 0, 1,
     50, -50,  50,  1, 1, 0, 1,
    -50,  50,  50,  1, 1, 0, 1,
    -50,  50,  50,  1, 1, 0, 1,
     50, -50,  50,  1, 1, 0, 1,
     50,  50,  50,  1, 1, 0, 1
  );

var
  ContextSettings: sfContextSettings;
  Mode: sfVideoMode;
  Window: PsfWindow;
  Title: string;
  Ratio: GLfloat;
  Clock: PsfClock;
  Event: sfEvent;
begin
{$ifdef LINUX}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

{$endif}
  // Request a 32-bits depth buffer when creating the window
  ContextSettings.DepthBits := 32;

  Mode.width := 640;
  Mode.height := 480;
  // Create the main window
  Title := 'SFML window with OpenGL - ' + lowerCase({$I %FPCTARGETCPU%}) + '-' + lowerCase({$I %FPCTARGETOS%});
  Window := sfWindow_create(Mode, PChar(Title), sfUint32(sfDefaultStyle), @ContextSettings);

  // Make it the active window for OpenGL calls
  sfWindow_setActive(Window, sfTrue);

  // Set the color and depth clear values
  glClearDepth(1);
  glClearColor(0, 0, 0, 1);

  // Enable Z-buffer read and write
  glEnable(GL_DEPTH_TEST);
  glDepthMask(GL_TRUE);

  // Disable lighting and texturing
  glDisable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);

  // Configure the viewport (the same size as the window)
  glViewport(0, 0, sfWindow_getSize(Window).X, sfWindow_getSize(Window).Y);

  // Setup a perspective projection
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  Ratio := sfWindow_getSize(Window).X / sfWindow_getSize(Window).Y;
  glFrustum(-Ratio, Ratio, -1, 1, 1, 500);

  // Enable position and color vertex components
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);
  glVertexPointer(3, GL_FLOAT, 7 * sizeof(GLfloat), @Cube);
  glColorPointer(4, GL_FLOAT, 7 * sizeof(GLfloat), @Cube[3]);

  // Disable normal and texture coordinates vertex components
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);

  // Create a clock for measuring the time elapsed
  Clock := sfClock_create();

  // Start the game loop
  while sfWindow_isOpen(Window) = sfTrue do
  begin
    // Process events
    while sfWindow_pollEvent(Window, @Event) = sfTrue do
    begin
      // Close window: exit
      if Event.type_ = sfEvtClosed then
        sfWindow_close(Window);

      // Escape key: exit
      if (Event.type_ = sfEvtKeyPressed) and (Event.key.code = sfKeyEscape) then
        sfWindow_close(Window);

      // Resize event: adjust the viewport
      if (Event.type_ = sfEvtResized) then
        glViewport(0, 0, Event.size.width, Event.size.height);
    end;

    // Clear the color and depth buffers
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    // Apply some transformations to rotate the cube
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(0, 0, -200);
    glRotatef(sfTime_AsSeconds(sfClock_getElapsedTime(Clock)) * 50, 1, 0, 0);
    glRotatef(sfTime_AsSeconds(sfClock_getElapsedTime(Clock)) * 30, 0, 1, 0);
    glRotatef(sfTime_AsSeconds(sfClock_getElapsedTime(Clock)) * 90, 0, 0, 1);

    // Draw the cube
    glDrawArrays(GL_TRIANGLES, 0, 36);

    // Finally, display the rendered frame on screen
    sfWindow_display(Window);
  end;

  sfWindow_Destroy(Window);
end.
