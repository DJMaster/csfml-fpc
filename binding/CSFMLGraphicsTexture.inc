//
// SFML/CSFML Graphics.h header binding for the Free Pascal Compiler aka FPC
//
// Binaries and demos available at http://www.djmaster.com/
//

////////////////////////////////////////////////////////////
//
// SFML - Simple and Fast Multimedia Library
// Copyright (C) 2007-2015 Laurent Gomila (laurent@sfml-dev.org)
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

// #ifndef SFML_TEXTURE_H
// #define SFML_TEXTURE_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/Graphics/Export.h>
// #include <SFML/Graphics/Rect.h>
// #include <SFML/Graphics/Types.h>
// #include <SFML/Window/Types.h>
// #include <SFML/System/InputStream.h>
// #include <SFML/System/Vector2.h>
// #include <stddef.h>


////////////////////////////////////////////////////////////
/// \brief Create a new texture
///
/// \param width  Texture width
/// \param height Texture height
///
/// \return A new sfTexture object, or NULL if it failed
///
////////////////////////////////////////////////////////////
function sfTexture_create(width: cuint; height: cuint): PsfTexture; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Create a new texture from a file
///
/// \param filename Path of the image file to load
/// \param area     Area of the source image to load (NULL to load the entire image)
///
/// \return A new sfTexture object, or NULL if it failed
///
////////////////////////////////////////////////////////////
function sfTexture_createFromFile(const filename: pchar; const area: PsfIntRect): PsfTexture; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Create a new texture from a file in memory
///
/// \param data        Pointer to the file data in memory
/// \param sizeInBytes Size of the data to load, in bytes
/// \param area        Area of the source image to load (NULL to load the entire image)
///
/// \return A new sfTexture object, or NULL if it failed
///
////////////////////////////////////////////////////////////
function sfTexture_createFromMemory(const data: pointer; sizeInBytes: csize_t; const area: PsfIntRect): PsfTexture; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Create a new texture from a custom stream
///
/// \param stream Source stream to read from
/// \param area   Area of the source image to load (NULL to load the entire image)
///
/// \return A new sfTexture object, or NULL if it failed
///
////////////////////////////////////////////////////////////
function sfTexture_createFromStream(stream: PsfInputStream; const area: PsfIntRect): PsfTexture; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Create a new texture from an image
///
/// \param image Image to upload to the texture
/// \param area  Area of the source image to load (NULL to load the entire image)
///
/// \return A new sfTexture object, or NULL if it failed
///
////////////////////////////////////////////////////////////
function sfTexture_createFromImage(const image: PsfImage; const area: PsfIntRect): PsfTexture; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Copy an existing texture
///
/// \param texture Texture to copy
///
/// \return Copied object
///
////////////////////////////////////////////////////////////
function sfTexture_copy(const texture: PsfTexture): PsfTexture; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Destroy an existing texture
///
/// \param texture Texture to delete
///
////////////////////////////////////////////////////////////
procedure sfTexture_destroy(texture: PsfTexture); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Return the size of the texture
///
/// \param texture Texture to read
///
/// \return Size in pixels
///
////////////////////////////////////////////////////////////
function sfTexture_getSize(const texture: PsfTexture): sfVector2u; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Copy a texture's pixels to an image
///
/// \param texture Texture to copy
///
/// \return Image containing the texture's pixels
///
////////////////////////////////////////////////////////////
function sfTexture_copyToImage(const texture: PsfTexture): PsfImage; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Update a texture from an array of pixels
///
/// \param texture Texture to update
/// \param pixels  Array of pixels to copy to the texture
/// \param width   Width of the pixel region contained in \a pixels
/// \param height  Height of the pixel region contained in \a pixels
/// \param x       X offset in the texture where to copy the source pixels
/// \param y       Y offset in the texture where to copy the source pixels
///
////////////////////////////////////////////////////////////
procedure sfTexture_updateFromPixels(texture: PsfTexture; const pixels: PsfUint8; width: cuint; height: cuint; x: cuint; y: cuint); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Update a texture from an image
///
/// \param texture Texture to update
/// \param image   Image to copy to the texture
/// \param x       X offset in the texture where to copy the source pixels
/// \param y       Y offset in the texture where to copy the source pixels
///
////////////////////////////////////////////////////////////
procedure sfTexture_updateFromImage(texture: PsfTexture; const image: PsfImage; x: cuint; y: cuint); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Update a texture from the contents of a window
///
/// \param texture Texture to update
/// \param window  Window to copy to the texture
/// \param x       X offset in the texture where to copy the source pixels
/// \param y       Y offset in the texture where to copy the source pixels
///
////////////////////////////////////////////////////////////
procedure sfTexture_updateFromWindow(texture: PsfTexture; const window: PsfWindow; x: cuint; y: cuint); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Update a texture from the contents of a render-window
///
/// \param texture      Texture to update
/// \param renderWindow Render-window to copy to the texture
/// \param x            X offset in the texture where to copy the source pixels
/// \param y            Y offset in the texture where to copy the source pixels
///
////////////////////////////////////////////////////////////
procedure sfTexture_updateFromRenderWindow(texture: PsfTexture; const renderWindow: PsfRenderWindow; x: cuint; y: cuint); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Enable or disable the smooth filter on a texture
///
/// \param texture The texture object
/// \param smooth  sfTrue to enable smoothing, sfFalse to disable it
///
////////////////////////////////////////////////////////////
procedure sfTexture_setSmooth(texture: PsfTexture; smooth: sfBool); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Tell whether the smooth filter is enabled or not for a texture
///
/// \param texture The texture object
///
/// \return sfTrue if smoothing is enabled, sfFalse if it is disabled
///
////////////////////////////////////////////////////////////
function sfTexture_isSmooth(const texture: PsfTexture): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Enable or disable repeating for a texture
///
/// Repeating is involved when using texture coordinates
/// outside the texture rectangle [0, 0, width, height].
/// In this case, if repeat mode is enabled, the whole texture
/// will be repeated as many times as needed to reach the
/// coordinate (for example, if the X texture coordinate is
/// 3 * width, the texture will be repeated 3 times).
/// If repeat mode is disabled, the "extra space" will instead
/// be filled with border pixels.
/// Warning: on very old graphics cards, white pixels may appear
/// when the texture is repeated. With such cards, repeat mode
/// can be used reliably only if the texture has power-of-two
/// dimensions (such as 256x128).
/// Repeating is disabled by default.
///
/// \param texture  The texture object
/// \param repeated True to repeat the texture, false to disable repeating
///
////////////////////////////////////////////////////////////
procedure sfTexture_setRepeated(texture: PsfTexture; repeated: sfBool); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Tell whether a texture is repeated or not
///
/// \param texture The texture object
///
/// \return sfTrue if repeat mode is enabled, sfFalse if it is disabled
///
////////////////////////////////////////////////////////////
function sfTexture_isRepeated(const texture: PsfTexture): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Get the underlying OpenGL handle of the texture.
///
/// You shouldn't need to use this function, unless you have
/// very specific stuff to implement that SFML doesn't support,
/// or implement a temporary workaround until a bug is fixed.
///
/// \param texture The texture object
///
/// \return OpenGL handle of the texture or 0 if not yet created
///
////////////////////////////////////////////////////////////
function sfTexture_getNativeHandle(const texture: PsfTexture): cuint; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Bind a texture for rendering
///
/// This function is not part of the graphics API, it mustn't be
/// used when drawing SFML entities. It must be used only if you
/// mix sfTexture with OpenGL code.
///
/// \code
/// sfTexture *t1, *t2;
/// ...
/// sfTexture_bind(t1);
/// // draw OpenGL stuff that use t1...
/// sfTexture_bind(t2);
/// // draw OpenGL stuff that use t2...
/// sfTexture_bind(NULL);
/// // draw OpenGL stuff that use no texture...
/// \endcode
///
/// \param texture Pointer to the texture to bind, can be null to use no texture
///
////////////////////////////////////////////////////////////
procedure sfTexture_bind(const texture: PsfTexture); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Get the maximum texture size allowed
///
/// \return Maximum size allowed for textures, in pixels
///
////////////////////////////////////////////////////////////
function sfTexture_getMaximumSize(): cuint; cdecl; external LIB_CSFMLGRAPHICS;


// #endif // SFML_TEXTURE_H