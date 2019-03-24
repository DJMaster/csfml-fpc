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

// #ifndef SFML_RENDERTEXTURE_H
// #define SFML_RENDERTEXTURE_H

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
// #include <SFML/Window/Window.h>
// #include <SFML/System/Vector2.h>
// #include <stddef.h>

(*
  \brief Construct a new render texture

  \param width       Width of the render texture
  \param height      Height of the render texture
  \param depthBuffer Do you want a depth-buffer attached? (useful only if you're doing 3D OpenGL on the rendertexture)

  \return A new sfRenderTexture object, or NULL if it failed

  \deprecated
  Use sfRenderTexture_createWithSettings instead.
*)
function sfRenderTexture_create(width: cuint; height: cuint; depthBuffer: sfBool): PsfRenderTexture; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Construct a new render texture

  \param width    Width of the render texture
  \param height   Height of the render texture
  \param settings Settings of the render texture

  \return A new sfRenderTexture object, or NULL if it failed
*)
function sfRenderTexture_createWithSettings(width: cuint; height: cuint; settings: sfContextSettings): PsfRenderTexture; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Destroy an existing render texture

  \param renderTexture Render texture to destroy
*)
procedure sfRenderTexture_destroy(renderTexture: PsfRenderTexture); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the size of the rendering region of a render texture

  \param renderTexture Render texture object

  \return Size in pixels
*)
function sfRenderTexture_getSize(const renderTexture: PsfRenderTexture): sfVector2u; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Activate or deactivate a render texture as the current target for rendering

  \param renderTexture Render texture object
  \param active        sfTrue to activate, sfFalse to deactivate

  \return True if operation was successful, false otherwise
*)
function sfRenderTexture_setActive(renderTexture: PsfRenderTexture; active: sfBool): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Update the contents of the target texture

  \param renderTexture Render texture object
*)
procedure sfRenderTexture_display(renderTexture: PsfRenderTexture); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Clear the rendertexture with the given color

  \param renderTexture Render texture object
  \param color         Fill color
*)
procedure sfRenderTexture_clear(renderTexture: PsfRenderTexture; color: sfColor); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Change the current active view of a render texture

  \param renderTexture Render texture object
  \param view          Pointer to the new view
*)
procedure sfRenderTexture_setView(renderTexture: PsfRenderTexture; const view: PsfView); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the current active view of a render texture

  \param renderTexture Render texture object

  \return Current active view
*)
function sfRenderTexture_getView(const renderTexture: PsfRenderTexture): PsfView; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the default view of a render texture

  \param renderTexture Render texture object

  \return Default view of the rendertexture
*)
function sfRenderTexture_getDefaultView(const renderTexture: PsfRenderTexture): PsfView; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the viewport of a view applied to this target

  \param renderTexture Render texture object
  \param view          Target view

  \return Viewport rectangle, expressed in pixels in the current target
*)
function sfRenderTexture_getViewport(const renderTexture: PsfRenderTexture; const view: PsfView): sfIntRect; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Convert a point from texture coordinates to world coordinates

  This function finds the 2D position that matches the
  given pixel of the render-texture. In other words, it does
  the inverse of what the graphics card does, to find the
  initial position of a rendered pixel.

  Initially, both coordinate systems (world units and target pixels)
  match perfectly. But if you define a custom view or resize your
  render-texture, this assertion is not true anymore, ie. a point
  located at (10, 50) in your render-texture may map to the point
  (150, 75) in your 2D world -- if the view is translated by (140, 25).

  This version uses a custom view for calculations, see the other
  overload of the function if you want to use the current view of the
  render-texture.

  \param renderTexture Render texture object
  \param point Pixel to convert
  \param view The view to use for converting the point

  \return The converted point, in "world" units
*)
function sfRenderTexture_mapPixelToCoords(const renderTexture: PsfRenderTexture; point: sfVector2i; const view: PsfView): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Convert a point from world coordinates to texture coordinates

  This function finds the pixel of the render-texture that matches
  the given 2D point. In other words, it goes through the same process
  as the graphics card, to compute the final position of a rendered point.

  Initially, both coordinate systems (world units and target pixels)
  match perfectly. But if you define a custom view or resize your
  render-texture, this assertion is not true anymore, ie. a point
  located at (150, 75) in your 2D world may map to the pixel
  (10, 50) of your render-texture -- if the view is translated by (140, 25).

  This version uses a custom view for calculations, see the other
  overload of the function if you want to use the current view of the
  render-texture.

  \param renderTexture Render texture object
  \param point         Point to convert
  \param view          The view to use for converting the point

  \return The converted point, in target coordinates (pixels)
*)
function sfRenderTexture_mapCoordsToPixel(const renderTexture: PsfRenderTexture; point: sfVector2f; const view: PsfView): sfVector2i; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Draw a drawable object to the render-target

  \param renderTexture Render texture object
  \param object        Object to draw
  \param states        Render states to use for drawing (NULL to use the default states)
*)
procedure sfRenderTexture_drawSprite(renderTexture: PsfRenderTexture; const object_: PsfSprite; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderTexture_drawText(renderTexture: PsfRenderTexture; const object_: PsfText; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderTexture_drawShape(renderTexture: PsfRenderTexture; const object_: PsfShape; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderTexture_drawCircleShape(renderTexture: PsfRenderTexture; const object_: PsfCircleShape; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderTexture_drawConvexShape(renderTexture: PsfRenderTexture; const object_: PsfConvexShape; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderTexture_drawRectangleShape(renderTexture: PsfRenderTexture; const object_: PsfRectangleShape; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderTexture_drawVertexArray(renderTexture: PsfRenderTexture; const object_: PsfVertexArray; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;
procedure sfRenderTexture_drawVertexBuffer(renderTexture: PsfRenderTexture; const object_: PsfVertexBuffer; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Draw primitives defined by an array of vertices to a render texture

  \param renderTexture Render texture object
  \param vertices      Pointer to the vertices
  \param vertexCount   Number of vertices in the array
  \param type          Type of primitives to draw
  \param states        Render states to use for drawing (NULL to use the default states)
*)
procedure sfRenderTexture_drawPrimitives(renderTexture: PsfRenderTexture; const vertices: PsfVertex; vertexCount: csize_t; type_: sfPrimitiveType; const states: PsfRenderStates); cdecl; external LIB_CSFMLGRAPHICS;

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

  \param renderTexture Render texture object
*)
procedure sfRenderTexture_pushGLStates(renderTexture: PsfRenderTexture); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Restore the previously saved OpenGL render states and matrices

  See the description of pushGLStates to get a detailed
  description of these functions.

  \param renderTexture Render texture object
*)
procedure sfRenderTexture_popGLStates(renderTexture: PsfRenderTexture); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Reset the internal OpenGL states so that the target is ready for drawing

  This function can be used when you mix SFML drawing
  and direct OpenGL rendering, if you choose not to use
  pushGLStates/popGLStates. It makes sure that all OpenGL
  states needed by SFML are set, so that subsequent sfRenderTexture_draw*()
  calls will work as expected.

  \param renderTexture Render texture object
*)
procedure sfRenderTexture_resetGLStates(renderTexture: PsfRenderTexture); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the target texture of a render texture

  \param renderTexture Render texture object

  \return Pointer to the target texture
*)
function sfRenderTexture_getTexture(const renderTexture: PsfRenderTexture): PsfTexture; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the maximum anti-aliasing level supported by the system

  \return The maximum anti-aliasing level supported by the system
*)
function sfRenderTexture_getMaximumAntialiasingLevel(): cuint; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Enable or disable the smooth filter on a render texture

  \param renderTexture Render texture object
  \param smooth        sfTrue to enable smoothing, sfFalse to disable it
*)
procedure sfRenderTexture_setSmooth(renderTexture: PsfRenderTexture; smooth: sfBool); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Tell whether the smooth filter is enabled or not for a render texture

  \param renderTexture Render texture object

  \return sfTrue if smoothing is enabled, sfFalse if it is disabled
*)
function sfRenderTexture_isSmooth(const renderTexture: PsfRenderTexture): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Enable or disable texture repeating

  \param renderTexture Render texture object
  \param repeated      sfTrue to enable repeating, sfFalse to disable it
*)
procedure sfRenderTexture_setRepeated(renderTexture: PsfRenderTexture; repeated: sfBool); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Tell whether the texture is repeated or not

  \param renderTexture Render texture object

  \return sfTrue if repeat mode is enabled, sfFalse if it is disabled
*)
function sfRenderTexture_isRepeated(const renderTexture: PsfRenderTexture): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Generate a mipmap using the current texture data

  This function is similar to sfTexture_generateMipmap and operates
  on the texture used as the target for drawing.
  Be aware that any draw operation may modify the base level image data.
  For this reason, calling this function only makes sense after all
  drawing is completed and display has been called. Not calling display
  after subsequent drawing will lead to undefined behavior if a mipmap
  had been previously generated.

  \return sfTrue if mipmap generation was successful, sfFalse if unsuccessful
*)
function sfRenderTexture_generateMipmap(renderTexture: PsfRenderTexture): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

// #endif // SFML_RENDERTEXTURE_H
