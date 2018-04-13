program OpenGL;

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
  // Define a 3D cube (6 faces made of 2 triangles composed by 3 vertices)
  CCube: array [0.. 179] of GLfloat = (
      // positions    // texture coordinates
      -20, -20, -20,  0, 0,
      -20,  20, -20,  1, 0,
      -20, -20,  20,  0, 1,
      -20, -20,  20,  0, 1,
      -20,  20, -20,  1, 0,
      -20,  20,  20,  1, 1,

       20, -20, -20,  0, 0,
       20,  20, -20,  1, 0,
       20, -20,  20,  0, 1,
       20, -20,  20,  0, 1,
       20,  20, -20,  1, 0,
       20,  20,  20,  1, 1,

      -20, -20, -20,  0, 0,
       20, -20, -20,  1, 0,
      -20, -20,  20,  0, 1,
      -20, -20,  20,  0, 1,
       20, -20, -20,  1, 0,
       20, -20,  20,  1, 1,

      -20,  20, -20,  0, 0,
       20,  20, -20,  1, 0,
      -20,  20,  20,  0, 1,
      -20,  20,  20,  0, 1,
       20,  20, -20,  1, 0,
       20,  20,  20,  1, 1,

      -20, -20, -20,  0, 0,
       20, -20, -20,  1, 0,
      -20,  20, -20,  0, 1,
      -20,  20, -20,  0, 1,
       20, -20, -20,  1, 0,
       20,  20, -20,  1, 1,

      -20, -20,  20,  0, 0,
       20, -20,  20,  1, 0,
      -20,  20,  20,  0, 1,
      -20,  20,  20,  0, 1,
       20, -20,  20,  1, 0,
       20,  20,  20,  1, 1
  );
  
var
  ContextSettings: sfContextSettings;
  Mode: sfVideoMode;
  Window: PsfRenderWindow;
  Title: string;
  BackgroundTexture: PsfTexture;
  Sprite: PsfSprite;
  Font: PsfFont;
  Position: sfVector2f;
  Text: PsfText;
  Texture: GLuint;
  Ratio: GLfloat;
  Clock: PsfClock;
  Event: sfEvent;
  Image: PsfImage;
  x,y : cfloat;
begin
{$ifdef LINUX}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

{$endif}
  // Request a 24-bits depth buffer when creating the window
  ContextSettings.DepthBits := 24;

  Mode.width := 800;
  Mode.height := 600;
  // Create the main window
  Title := 'SFML graphics with OpenGL - ' + lowerCase({$I %FPCTARGETCPU%}) + '-' + lowerCase({$I %FPCTARGETOS%});
  Window := sfRenderWindow_create(Mode, PChar(Title), sfUint32(sfDefaultStyle), @ContextSettings);
  sfRenderWindow_setVerticalSyncEnabled(Window, sfTrue);

  // Create a sprite for the background
  BackgroundTexture := sfTexture_createFromFile('../resources/background.jpg', nil);	
  if (BackgroundTexture = nil) then halt(1);
  Sprite := sfSprite_create();
  sfSprite_setTexture(Sprite, BackgroundTexture, sfTrue);

  // Create some text to draw on top of our OpenGL object
  Font := sfFont_createFromFile('../resources/sansation.ttf');
  if (Font = nil) then halt(1);

  Text := sfText_create();
  sfText_setString(Text, 'SFML / OpenGL demo');
  sfText_setFont(Text, Font);
  sfText_setColor(Text, sfColor_fromRGBA(255, 255, 255, 170));
  Position.x := 250;
  Position.y := 450;
  sfText_setPosition(Text, Position);

  // Make the window the active target for OpenGL calls
  // Note: If using sf::Texture or sf::Shader with OpenGL,
  // be sure to call sf::Texture::getMaximumSize() and/or
  // sf::Shader::isAvailable() at least once before calling
  // setActive(), as those functions will cause a context switch
  sfRenderWindow_setActive(Window, sfTrue);

  // Load an OpenGL texture.
  // We could directly use a sf::Texture as an OpenGL texture (with its Bind() member function),
  // but here we want more control on it (generate mipmaps, ...) so we create a new one from an image
  Texture := 0;
  Image := sfImage_createFromFile('../resources/texture.jpg');
  if (Image = nil) then halt(1);
  glGenTextures(1, @Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, sfImage_getSize(Image).x, sfImage_getSize(Image).y, 0, GL_RGBA, GL_UNSIGNED_BYTE, sfImage_getPixelsPtr(Image));
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  // Enable Z-buffer read and write
  glEnable(GL_DEPTH_TEST);
  glDepthMask(GL_TRUE);
  glClearDepth(1.0);

  // Disable lighting
  glDisable(GL_LIGHTING);

  // Configure the viewport (the same size as the window)
  glViewport(0, 0, sfRenderWindow_getSize(Window).x, sfRenderWindow_getSize(Window).y);

  // Setup a perspective projection
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  Ratio := sfRenderWindow_getSize(Window).x / sfRenderWindow_getSize(Window).y;
  glFrustum(-ratio, ratio, -1.0, 1.0, 1.0, 500.0);

  // Bind the texture
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, texture);

  // Enable position and texture coordinates vertex components
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glVertexPointer(3, GL_FLOAT, 5 * sizeof(GLfloat), @CCube);
  glTexCoordPointer(2, GL_FLOAT, 5 * sizeof(GLfloat), @CCube[3]);

  // Disable normal and color vertex components
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);

  // Create a clock for measuring the time elapsed
  Clock := sfClock_create();

  // Start game loop
  while sfRenderWindow_isOpen(Window) = sfTrue do
  begin
    // Process events
    while sfRenderWindow_pollEvent(Window, @Event) = sfTrue do
    begin
      // Close window: exit
      if Event.type_ = sfEvtClosed then
        sfRenderWindow_close(Window);

      // Escape key: exit
      if (Event.type_ = sfEvtKeyPressed) and (Event.key.code = sfKeyEscape) then
        sfRenderWindow_close(Window);

      // Resize event: adjust the viewport
      if (Event.type_ = sfEvtResized) then
        glViewport(0, 0, Event.size.width, Event.size.height);
    end;

    // Draw the background
    sfRenderWindow_pushGLStates(Window);
    sfRenderWindow_drawSprite(Window, Sprite, nil);
    sfRenderWindow_popGLStates(Window);
  
    // Clear the depth buffer
    glClear(GL_DEPTH_BUFFER_BIT);
  
    // We get the position of the mouse cursor, so that we can move the box accordingly
    x :=  sfMouse_getPositionRenderWindow(Window).x * 200 / sfRenderWindow_getSize(Window).x - 100;
    y := -sfMouse_getPositionRenderWindow(Window).y * 200 / sfRenderWindow_getSize(Window).y + 100;
  
    // Apply some transformations
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(x, y, -100);
    glRotatef(sfTime_asSeconds(sfClock_getElapsedTime(Clock)) * 50, 1, 0, 0);
    glRotatef(sfTime_asSeconds(sfClock_getElapsedTime(Clock)) * 30, 0, 1, 0);
    glRotatef(sfTime_asSeconds(sfClock_getElapsedTime(Clock)) * 90, 0, 0, 1);
  
    // Draw the cube
    glDrawArrays(GL_TRIANGLES, 0, 36);
  
    // Draw some text on top of our OpenGL object
    sfRenderWindow_pushGLStates(Window);
    sfRenderWindow_drawText(Window, Text, nil);
    sfRenderWindow_popGLStates(Window);
  
    // Finally, display the rendered frame on screen
    sfRenderWindow_display(Window);
  end;

  // Don't forget to destroy our texture
  glDeleteTextures(1, @Texture);
  sfRenderWindow_Destroy(Window);
end.
