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

// #ifndef SFML_SPRITE_H
// #define SFML_SPRITE_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/Graphics/Export.h>
// #include <SFML/Graphics/BlendMode.h>
// #include <SFML/Graphics/Color.h>
// #include <SFML/Graphics/Rect.h>
// #include <SFML/Graphics/Transform.h>
// #include <SFML/Graphics/Types.h>
// #include <SFML/System/Vector2.h>

(*
  \brief Create a new sprite

  \return A new sfSprite object, or NULL if it failed
*)
function sfSprite_create(): PsfSprite; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Copy an existing sprite

  \param sprite Sprite to copy

  \return Copied object
*)
function sfSprite_copy(const sprite: PsfSprite): PsfSprite; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Destroy an existing sprite

  \param sprite Sprite to delete
*)
procedure sfSprite_destroy(sprite: PsfSprite); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the position of a sprite

  This function completely overwrites the previous position.
  See sfSprite_move to apply an offset based on the previous position instead.
  The default position of a sprite Sprite object is (0, 0).

  \param sprite   Sprite object
  \param position New position
*)
procedure sfSprite_setPosition(sprite: PsfSprite; position: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the orientation of a sprite

  This function completely overwrites the previous rotation.
  See sfSprite_rotate to add an angle based on the previous rotation instead.
  The default rotation of a sprite Sprite object is 0.

  \param sprite Sprite object
  \param angle  New rotation, in degrees
*)
procedure sfSprite_setRotation(sprite: PsfSprite; angle: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the scale factors of a sprite

  This function completely overwrites the previous scale.
  See sfSprite_scale to add a factor based on the previous scale instead.
  The default scale of a sprite Sprite object is (1, 1).

  \param sprite Sprite object
  \param scale  New scale factors
*)
procedure sfSprite_setScale(sprite: PsfSprite; scale: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the local origin of a sprite

  The origin of an object defines the center point for
  all transformations (position, scale, rotation).
  The coordinates of this point must be relative to the
  top-left corner of the object, and ignore all
  transformations (position, scale, rotation).
  The default origin of a sprite Sprite object is (0, 0).

  \param sprite Sprite object
  \param origin New origin
*)
procedure sfSprite_setOrigin(sprite: PsfSprite; origin: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the position of a sprite

  \param sprite Sprite object

  \return Current position
*)
function sfSprite_getPosition(const sprite: PsfSprite): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the orientation of a sprite

  The rotation is always in the range [0, 360].

  \param sprite Sprite object

  \return Current rotation, in degrees
*)
function sfSprite_getRotation(const sprite: PsfSprite): cfloat; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the current scale of a sprite

  \param sprite Sprite object

  \return Current scale factors
*)
function sfSprite_getScale(const sprite: PsfSprite): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the local origin of a sprite

  \param sprite Sprite object

  \return Current origin
*)
function sfSprite_getOrigin(const sprite: PsfSprite): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Move a sprite by a given offset

  This function adds to the current position of the object,
  unlike sfSprite_setPosition which overwrites it.

  \param sprite Sprite object
  \param offset Offset
*)
procedure sfSprite_move(sprite: PsfSprite; offset: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Rotate a sprite

  This function adds to the current rotation of the object,
  unlike sfSprite_setRotation which overwrites it.

  \param sprite Sprite object
  \param angle  Angle of rotation, in degrees
*)
procedure sfSprite_rotate(sprite: PsfSprite; angle: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Scale a sprite

  This function multiplies the current scale of the object,
  unlike sfSprite_setScale which overwrites it.

  \param sprite  Sprite object
  \param factors Scale factors
*)
procedure sfSprite_scale(sprite: PsfSprite; factors: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the combined transform of a sprite

  \param sprite Sprite object

  \return Transform combining the position/rotation/scale/origin of the object
*)
function sfSprite_getTransform(const sprite: PsfSprite): sfTransform; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the inverse of the combined transform of a sprite

  \param sprite Sprite object

  \return Inverse of the combined transformations applied to the object
*)
function sfSprite_getInverseTransform(const sprite: PsfSprite): sfTransform; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Change the source texture of a sprite

  The \a texture argument refers to a texture that must
  exist as long as the sprite uses it. Indeed, the sprite
  doesn't store its own copy of the texture, but rather keeps
  a pointer to the one that you passed to this function.
  If the source texture is destroyed and the sprite tries to
  use it, the behaviour is undefined.
  If \a resetRect is true, the TextureRect property of
  the sprite is automatically adjusted to the size of the new
  texture. If it is false, the texture rect is left unchanged.

  \param sprite    Sprite object
  \param texture   New texture
  \param resetRect Should the texture rect be reset to the size of the new texture?
*)
procedure sfSprite_setTexture(sprite: PsfSprite; const texture: PsfTexture; resetRect: sfBool); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the sub-rectangle of the texture that a sprite will display

  The texture rect is useful when you don't want to display
  the whole texture, but rather a part of it.
  By default, the texture rect covers the entire texture.

  \param sprite    Sprite object
  \param rectangle Rectangle defining the region of the texture to display
*)
procedure sfSprite_setTextureRect(sprite: PsfSprite; rect: sfIntRect); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the global color of a sprite

  This color is modulated (multiplied) with the sprite's
  texture. It can be used to colorize the sprite, or change
  its global opacity.
  By default, the sprite's color is opaque white.

  \param sprite Sprite object
  \param color  New color of the sprite
*)
procedure sfSprite_setColor(sprite: PsfSprite; color: sfColor); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the source texture of a sprite

  If the sprite has no source texture, a NULL pointer is returned.
  The returned pointer is const, which means that you can't
  modify the texture when you retrieve it with this function.

  \param sprite Sprite object

  \return Pointer to the sprite's texture
*)
function sfSprite_getTexture(const sprite: PsfSprite): PsfTexture; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the sub-rectangle of the texture displayed by a sprite

  \param sprite Sprite object

  \return Texture rectangle of the sprite
*)
function sfSprite_getTextureRect(const sprite: PsfSprite): sfIntRect; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the global color of a sprite

  \param sprite Sprite object

  \return Global color of the sprite
*)
function sfSprite_getColor(const sprite: PsfSprite): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the local bounding rectangle of a sprite

  The returned rectangle is in local coordinates, which means
  that it ignores the transformations (translation, rotation,
  scale, ...) that are applied to the entity.
  In other words, this function returns the bounds of the
  entity in the entity's coordinate system.

  \param sprite Sprite object

  \return Local bounding rectangle of the entity
*)
function sfSprite_getLocalBounds(const sprite: PsfSprite): sfFloatRect; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the global bounding rectangle of a sprite

  The returned rectangle is in global coordinates, which means
  that it takes in account the transformations (translation,
  rotation, scale, ...) that are applied to the entity.
  In other words, this function returns the bounds of the
  sprite in the global 2D world's coordinate system.

  \param sprite Sprite object

  \return Global bounding rectangle of the entity
*)
function sfSprite_getGlobalBounds(const sprite: PsfSprite): sfFloatRect; cdecl; external LIB_CSFMLGRAPHICS;

// #endif // SFML_SPRITE_H
