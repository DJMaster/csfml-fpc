//
// SFML/CSFML Graphics.h header binding for the Free Pascal Compiler aka FPC
//
// Binaries and demos available at https://www.djmaster.com/
//

////////////////////////////////////////////////////////////
//
// SFML - Simple and Fast Multimedia Library
// Copyright (C) 2007-2018 Laurent Gomila (laurent@sfml-dev.org)
//
// This software is provided 'as-is', without any express or implied warranty.
// In no event will the authors be held liable for any damages arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it freely,
// subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented;
//    you must not claim that you wrote the original software.
//    If you use this software in a product, an acknowledgment
//    in the product documentation would be appreciated but is not required.
//
// 2. Altered source versions must be plainly marked as such,
//    and must not be misrepresented as being the original software.
//
// 3. This notice may not be removed or altered from any source distribution.
//
////////////////////////////////////////////////////////////

// #ifndef SFML_RENDERWINDOW_H
// #define SFML_RENDERWINDOW_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/Graphics/Export.h>
// #include <SFML/Graphics/Color.h>
// #include <SFML/Graphics/Rect.h>
// #include <SFML/Graphics/Types.h>
// #include <SFML/Graphics/PrimitiveType.h>
// #include <SFML/Graphics/RenderStates.h>
// #include <SFML/Graphics/Vertex.h>
// #include <SFML/Window/Event.h>
// #include <SFML/Window/VideoMode.h>
// #include <SFML/Window/WindowHandle.h>
// #include <SFML/Window/Window.h>
// #include <SFML/System/Vector2.h>
// #include <stddef.h>

(*
  \brief Construct a new render window

  \param mode     Video mode to use
  \param title    Title of the window
  \param style    Window style
  \param settings Creation settings (pass NULL to use default values)
*)
function sfRenderWindow_create(mode: sfVideoMode; const title: pchar; style: sfUint32; const settings: PsfContextSettings): PsfRenderWindow; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Construct a new render window (with a UTF-32 title)

  \param mode     Video mode to use
  \param title    Title of the window (UTF-32)
  \param style    Window style
  \param settings Creation settings (pass NULL to use default values)
*)
function sfRenderWindow_createUnicode(mode: sfVideoMode; const title: PsfUint32; style: sfUint32; const settings: PsfContextSettings): PsfRenderWindow; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Construct a render window from an existing control

  \param handle   Platform-specific handle of the control
  \param settings Creation settings (pass NULL to use default values)
*)
function sfRenderWindow_createFromHandle(handle: sfWindowHandle; const settings: PsfContextSettings): PsfRenderWindow; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Destroy an existing render window

  \param renderWindow Render window to destroy
*)
procedure sfRenderWindow_destroy(renderWindow: PsfRenderWindow); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Close a render window (but doesn't destroy the internal data)

  \param renderWindow Render window to close
*)
procedure sfRenderWindow_close(renderWindow: PsfRenderWindow); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Tell whether or not a render window is opened

  \param renderWindow Render window object
*)
function sfRenderWindow_isOpen(const renderWindow: PsfRenderWindow): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the creation settings of a render window

  \param renderWindow Render window object

  \return Settings used to create the window
*)
function sfRenderWindow_getSettings(const renderWindow: PsfRenderWindow): sfContextSettings; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the event on top of event queue of a render window, if any, and pop it

  \param renderWindow Render window object
  \param event        Event to fill, if any

  \return sfTrue if an event was returned, sfFalse if event queue was empty
*)
function sfRenderWindow_pollEvent(renderWindow: PsfRenderWindow; event: PsfEvent): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Wait for an event and return it

  \param renderWindow Render window object
  \param event        Event to fill

  \return sfFalse if an error occured
*)
function sfRenderWindow_waitEvent(renderWindow: PsfRenderWindow; event: PsfEvent): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the position of a render window

  \param renderWindow Render window object

  \return Position in pixels
*)
function sfRenderWindow_getPosition(const renderWindow: PsfRenderWindow): sfVector2i; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Change the position of a render window on screen

  Only works for top-level windows

  \param renderWindow Render window object
  \param position     New position, in pixels
*)
procedure sfRenderWindow_setPosition(renderWindow: PsfRenderWindow; position: sfVector2i); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the size of the rendering region of a render window

  \param renderWindow Render window object

  \return Size in pixels
*)
function sfRenderWindow_getSize(const renderWindow: PsfRenderWindow): sfVector2u; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Change the size of the rendering region of a render window

  \param renderWindow Render window object
  \param size         New size, in pixels
*)
procedure sfRenderWindow_setSize(renderWindow: PsfRenderWindow; size: sfVector2u); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Change the title of a render window

  \param renderWindow Render window object
  \param title        New title
*)
procedure sfRenderWindow_setTitle(renderWindow: PsfRenderWindow; const title: pchar); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Change the title of a render window (with a UTF-32 string)

  \param renderWindow Render window object
  \param title        New title
*)
procedure sfRenderWindow_setUnicodeTitle(renderWindow: PsfRenderWindow; const title: PsfUint32); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Change a render window's icon

  \param renderWindow Render window object
  \param width        Icon's width, in pixels
  \param height       Icon's height, in pixels
  \param pixels       Pointer to the pixels in memory, format must be RGBA 32 bits
*)
procedure sfRenderWindow_setIcon(renderWindow: PsfRenderWindow; width: cuint; height: cuint; const pixels: PsfUint8); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Show or hide a render window

  \param renderWindow Render window object
  \param visible      sfTrue to show the window, sfFalse to hide it
*)
procedure sfRenderWindow_setVisible(renderWindow: PsfRenderWindow; visible: sfBool); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Enable / disable vertical synchronization on a render window

  \param renderWindow Render window object
  \param enabled      sfTrue to enable v-sync, sfFalse to deactivate
*)
procedure sfRenderWindow_setVerticalSyncEnabled(renderWindow: PsfRenderWindow; enabled: sfBool); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Show or hide the mouse cursor on a render window

  \param renderWindow Render window object
  \param show         sfTrue to show, sfFalse to hide
*)
procedure sfRenderWindow_setMouseCursorVisible(renderWindow: PsfRenderWindow; show: sfBool); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Grab or release the mouse cursor

  If set, grabs the mouse cursor inside this window's client
  area so it may no longer be moved outside its bounds.
  Note that grabbing is only active while the window has
  focus and calling this function for fullscreen windows
  won't have any effect (fullscreen windows always grab the
  cursor).

  \param grabbed sfTrue to enable, sfFalse to disable
*)
procedure sfRenderWindow_setMouseCursorGrabbed(renderWindow: PsfRenderWindow; grabbed: sfBool); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the displayed cursor to a native system cursor

  Upon window creation, the arrow cursor is used by default.

  \warning The cursor must not be destroyed while in use by
           the window.

  \warning Features related to Cursor are not supported on
           iOS and Android.

  \param cursor Native system cursor type to display

  \see sfCursor_createFromSystem
  \see sfCursor_createFromPixels
*)
procedure sfRenderWindow_setMouseCursor(window: PsfRenderWindow; const cursor: PsfCursor); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Enable or disable automatic key-repeat for keydown events

  Automatic key-repeat is enabled by default

  \param renderWindow Render window object
  \param enabled      sfTrue to enable, sfFalse to disable
*)
procedure sfRenderWindow_setKeyRepeatEnabled(renderWindow: PsfRenderWindow; enabled: sfBool); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Limit the framerate to a maximum fixed frequency for a render window

  \param renderWindow Render window object
  \param limit        Framerate limit, in frames per seconds (use 0 to disable limit)
*)
procedure sfRenderWindow_setFramerateLimit(renderWindow: PsfRenderWindow; limit: cuint); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Change the joystick threshold, ie. the value below which no move event will be generated

  \param renderWindow Render window object
  \param threshold    New threshold, in range [0, 100]
*)
procedure sfRenderWindow_setJoystickThreshold(renderWindow: PsfRenderWindow; threshold: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Activate or deactivate a render window as the current target for rendering

  \param renderWindow Render window object
  \param active       sfTrue to activate, sfFalse to deactivate

  \return True if operation was successful, false otherwise
*)
function sfRenderWindow_setActive(renderWindow: PsfRenderWindow; active: sfBool): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

///////////////////////////////////////////////////////////
/// \brief Request the current render window to be made the active
/// foreground window
///
/// At any given time, only one window may have the input focus
/// to receive input events such as keystrokes or mouse events.
/// If a window requests focus, it only hints to the operating
/// system, that it would like to be focused. The operating system
/// is free to deny the request.
/// This is not to be confused with sfWindow_setActive().
///
///////////////////////////////////////////////////////////
procedure sfRenderWindow_requestFocus(renderWindow: PsfRenderWindow); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Check whether the render window has the input focus

  At any given time, only one window may have the input focus
  to receive input events such as keystrokes or most mouse
  events.

  \return True if window has focus, false otherwise
*)
function sfRenderWindow_hasFocus(const renderWindow: PsfRenderWindow): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Display a render window on screen

  \param renderWindow Render window object
*)
procedure sfRenderWindow_display(renderWindow: PsfRenderWindow); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Retrieve the OS-specific handle of a render window

  \param renderWindow Render window object

  \return Window handle
*)
function sfRenderWindow_getSystemHandle(const renderWindow: PsfRenderWindow): sfWindowHandle; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Clear a render window with the given color

  \param renderWindow Render window object
  \param color        Fill color
*)
procedure sfRenderWindow_clear(renderWindow: PsfRenderWindow; color: sfColor); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Change the current active view of a render window

  \param renderWindow Render window object
  \param view         Pointer to the new view
*)
procedure sfRenderWindow_setView(renderWindow: PsfRenderWindow; const view: PsfView); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the current active view of a render window

  \param renderWindow Render window object

  \return Current active view
*)
function sfRenderWindow_getView(const renderWindow: PsfRenderWindow): PsfView; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the default view of a render window

  \param renderWindow Render window object

  \return Default view of the render window
*)
function sfRenderWindow_getDefaultView(const renderWindow: PsfRenderWindow): PsfView; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the viewport of a view applied to this target

  \param renderWindow Render window object
  \param view         Target view

  \return Viewport rectangle, expressed in pixels in the current target
*)
function sfRenderWindow_getViewport(const renderWindow: PsfRenderWindow; const view: PsfView): sfIntRect; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Convert a point from window coordinates to world coordinates

  This function finds the 2D position that matches the
  given pixel of the render-window. In other words, it does
  the inverse of what the graphics card does, to find the
  initial position of a rendered pixel.

  Initially, both coordinate systems (world units and target pixels)
  match perfectly. But if you define a custom view or resize your
  render-window, this assertion is not true anymore, ie. a point
  located at (10, 50) in your render-window may map to the point
  (150, 75) in your 2D world -- if the view is translated by (140, 25).

  This function is typically used to find which point (or object) is
  located below the mouse cursor.

  This version uses a custom view for calculations, see the other
  overload of the function if you want to use the current view of the
  render-window.

  \param renderWindow Render window object
  \param point Pixel to convert
  \param view The view to use for converting the point

  \return The converted point, in "world" units
*)
function sfRenderWindow_mapPixelToCoords(const renderWindow: PsfRenderWindow; point: sfVector2i; const view: PsfView): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Convert a point from world coordinates to window coordinates

  This function finds the pixel of the render-window that matches
  the given 2D point. In other words, it goes through the same process
  as the graphics card, to compute the final position of a rendered point.

  Initially, both coordinate systems (world units and target pixels)
  match perfectly. But if you define a custom view or resize your
  render-window, this assertion is not true anymore, ie. a point
  located at (150, 75) in your 2D world may map to the pixel
  (10, 50) of your render-window -- if the view is translated by (140, 25).

  This version uses a custom view for calculations, see the other
  overload of the function if you want to use the current view of the
  render-window.

  \param renderWindow Render window object
  \param point Point to convert
  \param view The view to use for converting the point

  \return The converted point, in target coordinates (pixels)
*)
function sfRenderWindow_mapCoordsToPixel(const renderWindow: PsfRenderWindow; point: sfVector2f; const view: PsfView): sfVector2i; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Draw a drawable object to the render-target

  \param renderWindow render window object
  \param object       Object to draw
  \param states       Render states to use for drawing (NULL to use the default states)
*)
procedure sfRenderWindow_drawSprite(renderWindow: PsfRenderWindow; const object_: PsfSprite; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderWindow_drawText(renderWindow: PsfRenderWindow; const object_: PsfText; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderWindow_drawShape(renderWindow: PsfRenderWindow; const object_: PsfShape; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderWindow_drawCircleShape(renderWindow: PsfRenderWindow; const object_: PsfCircleShape; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderWindow_drawConvexShape(renderWindow: PsfRenderWindow; const object_: PsfConvexShape; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderWindow_drawRectangleShape(renderWindow: PsfRenderWindow; const object_: PsfRectangleShape; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderWindow_drawVertexArray(renderWindow: PsfRenderWindow; const object_: PsfVertexArray; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderWindow_drawVertexBuffer(renderWindow: PsfRenderWindow; const object_: PsfVertexBuffer; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Draw primitives defined by an array of vertices to a render window

  \param renderWindow render window object
  \param vertices     Pointer to the vertices
  \param vertexCount  Number of vertices in the array
  \param type         Type of primitives to draw
  \param states       Render states to use for drawing (NULL to use the default states)
*)
procedure sfRenderWindow_drawPrimitives(renderWindow: PsfRenderWindow; const vertices: PsfVertex; vertexCount: csize_t; type_: sfPrimitiveType; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Save the current OpenGL render states and matrices

  This function can be used when you mix SFML drawing
  and direct OpenGL rendering. Combined with popGLStates,
  it ensures that:
  \li SFML's internal states are not messed up by your OpenGL code
  \li your OpenGL states are not modified by a call to a SFML function

  Note that this function is quite expensive: it saves all the
  possible OpenGL states and matrices, even the ones you
  don't care about. Therefore it should be used wisely.
  It is provided for convenience, but the best results will
  be achieved if you handle OpenGL states yourself (because
  you know which states have really changed, and need to be
  saved and restored). Take a look at the resetGLStates
  function if you do so.

  \param renderWindow render window object
*)
procedure sfRenderWindow_pushGLStates(renderWindow: PsfRenderWindow); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Restore the previously saved OpenGL render states and matrices

  See the description of pushGLStates to get a detailed
  description of these functions.

  \param renderWindow render window object
*)
procedure sfRenderWindow_popGLStates(renderWindow: PsfRenderWindow); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Reset the internal OpenGL states so that the target is ready for drawing

  This function can be used when you mix SFML drawing
  and direct OpenGL rendering, if you choose not to use
  pushGLStates/popGLStates. It makes sure that all OpenGL
  states needed by SFML are set, so that subsequent sfRenderWindow_draw*()
  calls will work as expected.

  \param renderWindow render window object
*)
procedure sfRenderWindow_resetGLStates(renderWindow: PsfRenderWindow); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Copy the current contents of the window to an image

  \deprecated
  Use a sfTexture and its
  sfTexture_updateFromRenderWindow(sfTexture*, const sfRenderWindow*, unsigned int, unsigned int)
  function and copy its contents into an sfImage instead.
  \code
  sfVector2u windowSize = sfRenderWindow_getSize(window);
  sfTexture* texture = sfTexture_create(windowSize.x, windowSize.y);
  sfTexture_updateFromRenderWindow(texture, window, windowSize.x, windowSize.y);
  sfImage* screenshot = sfTexture_copyToImage(texture);
  \endcode

  This is a slow operation, whose main purpose is to make
  screenshots of the application. If you want to update an
  image with the contents of the window and then use it for
  drawing, you should rather use a sfTexture and the
  sfTexture_updateFromWindow(sfTexture*, const sfWindow*, unsigned int, unsigned int) function.
  You can also draw things directly to a texture with the
  sfRenderTexture class.

  \return sfImage containing the captured contents.
*)
function sfRenderWindow_capture(const renderWindow: PsfRenderWindow): PsfImage; cdecl; external LIB_CSFMLGRAPHICS; deprecated;

(*
  \brief Get the current position of the mouse relative to a render-window

  This function returns the current position of the mouse
  cursor relative to the given render-window, or desktop if NULL is passed.

  \param relativeTo Reference window

  \return Position of the mouse cursor, relative to the given render window
*)
function sfMouse_getPositionRenderWindow(const relativeTo: PsfRenderWindow): sfVector2i; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the current position of the mouse relative to a render window

  This function sets the current position of the mouse
  cursor relative to the given render-window, or desktop if NULL is passed.

  \param position   New position of the mouse
  \param relativeTo Reference window
*)
procedure sfMouse_setPositionRenderWindow(position: sfVector2i; const relativeTo: PsfRenderWindow); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the current position of a touch in window coordinates

  This function returns the current touch position
  relative to the given render window, or desktop if NULL is passed.

  \param finger Finger index
  \param relativeTo Reference window

  \return Current position of \a finger, or undefined if it's not down
*)
function sfTouch_getPositionRenderWindow(finger: cuint; const relativeTo: PsfRenderWindow): sfVector2i; cdecl; external LIB_CSFMLGRAPHICS;

// #endif // SFML_RENDERWINDOW_H
