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

// #ifndef SFML_CIRCLESHAPE_H
// #define SFML_CIRCLESHAPE_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include </Graphics/Export.h>
// #include </Graphics/Color.h>
// #include </Graphics/Rect.h>
// #include </Graphics/Transform.h>
// #include </Graphics/Types.h>
// #include </System/Vector2.h>
// #include <stddef.h>

(*
  \brief Create a new circle shape

  \return A new sfCircleShape object, or NULL if it failed
*)
function sfCircleShape_create(): PsfCircleShape; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Copy an existing circle shape

  \param shape Shape to copy

  \return Copied object
*)
function sfCircleShape_copy(const shape: PsfCircleShape): PsfCircleShape; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Destroy an existing circle Shape

  \param shape Shape to delete
*)
procedure sfCircleShape_destroy(shape: PsfCircleShape); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the position of a circle shape

  This function completely overwrites the previous position.
  See sfCircleShape_move to apply an offset based on the previous position instead.
  The default position of a circle Shape object is (0, 0).

  \param shape    Shape object
  \param position New position
*)
procedure sfCircleShape_setPosition(shape: PsfCircleShape; position: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the orientation of a circle shape

  This function completely overwrites the previous rotation.
  See sfCircleShape_rotate to add an angle based on the previous rotation instead.
  The default rotation of a circle Shape object is 0.

  \param shape Shape object
  \param angle New rotation, in degrees
*)
procedure sfCircleShape_setRotation(shape: PsfCircleShape; angle: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the scale factors of a circle shape

  This function completely overwrites the previous scale.
  See sfCircleShape_scale to add a factor based on the previous scale instead.
  The default scale of a circle Shape object is (1, 1).

  \param shape Shape object
  \param scale New scale factors
*)
procedure sfCircleShape_setScale(shape: PsfCircleShape; scale: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the local origin of a circle shape

  The origin of an object defines the center point for
  all transformations (position, scale, rotation).
  The coordinates of this point must be relative to the
  top-left corner of the object, and ignore all
  transformations (position, scale, rotation).
  The default origin of a circle Shape object is (0, 0).

  \param shape  Shape object
  \param origin New origin
*)
procedure sfCircleShape_setOrigin(shape: PsfCircleShape; origin: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the position of a circle shape

  \param shape Shape object

  \return Current position
*)
function sfCircleShape_getPosition(const shape: PsfCircleShape): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the orientation of a circle shape

  The rotation is always in the range [0, 360].

  \param shape Shape object

  \return Current rotation, in degrees
*)
function sfCircleShape_getRotation(const shape: PsfCircleShape): cfloat; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the current scale of a circle shape

  \param shape Shape object

  \return Current scale factors
*)
function sfCircleShape_getScale(const shape: PsfCircleShape): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the local origin of a circle shape

  \param shape Shape object

  \return Current origin
*)
function sfCircleShape_getOrigin(const shape: PsfCircleShape): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Move a circle shape by a given offset

  This function adds to the current position of the object,
  unlike sfCircleShape_setPosition which overwrites it.

  \param shape  Shape object
  \param offset Offset
*)
procedure sfCircleShape_move(shape: PsfCircleShape; offset: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Rotate a circle shape

  This function adds to the current rotation of the object,
  unlike sfCircleShape_setRotation which overwrites it.

  \param shape Shape object
  \param angle Angle of rotation, in degrees
*)
procedure sfCircleShape_rotate(shape: PsfCircleShape; angle: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Scale a circle shape

  This function multiplies the current scale of the object,
  unlike sfCircleShape_setScale which overwrites it.

  \param shape   Shape object
  \param factors Scale factors
*)
procedure sfCircleShape_scale(shape: PsfCircleShape; factors: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the combined transform of a circle shape

  \param shape Shape object

  \return Transform combining the position/rotation/scale/origin of the object
*)
function sfCircleShape_getTransform(const shape: PsfCircleShape): sfTransform; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the inverse of the combined transform of a circle shape

  \param shape Shape object

  \return Inverse of the combined transformations applied to the object
*)
function sfCircleShape_getInverseTransform(const shape: PsfCircleShape): sfTransform; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Change the source texture of a circle shape

  The \a texture argument refers to a texture that must
  exist as long as the shape uses it. Indeed, the shape
  doesn't store its own copy of the texture, but rather keeps
  a pointer to the one that you passed to this function.
  If the source texture is destroyed and the shape tries to
  use it, the behaviour is undefined.
  \a texture can be NULL to disable texturing.
  If \a resetRect is true, the TextureRect property of
  the shape is automatically adjusted to the size of the new
  texture. If it is false, the texture rect is left unchanged.

  \param shape     Shape object
  \param texture   New texture
  \param resetRect Should the texture rect be reset to the size of the new texture?
*)
procedure sfCircleShape_setTexture(shape: PsfCircleShape; const texture: PsfTexture; resetRect: sfBool); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the sub-rectangle of the texture that a circle shape will display

  The texture rect is useful when you don't want to display
  the whole texture, but rather a part of it.
  By default, the texture rect covers the entire texture.

  \param shape Shape object
  \param rect  Rectangle defining the region of the texture to display
*)
procedure sfCircleShape_setTextureRect(shape: PsfCircleShape; rect: sfIntRect); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the fill color of a circle shape

  This color is modulated (multiplied) with the shape's
  texture if any. It can be used to colorize the shape,
  or change its global opacity.
  You can use sfTransparent to make the inside of
  the shape transparent, and have the outline alone.
  By default, the shape's fill color is opaque white.

  \param shape Shape object
  \param color New color of the shape
*)
procedure sfCircleShape_setFillColor(shape: PsfCircleShape; color: sfColor); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the outline color of a circle shape

  You can use sfTransparent to disable the outline.
  By default, the shape's outline color is opaque white.

  \param shape Shape object
  \param color New outline color of the shape
*)
procedure sfCircleShape_setOutlineColor(shape: PsfCircleShape; color: sfColor); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the thickness of a circle shape's outline

  This number cannot be negative. Using zero disables
  the outline.
  By default, the outline thickness is 0.

  \param shape     Shape object
  \param thickness New outline thickness
*)
procedure sfCircleShape_setOutlineThickness(shape: PsfCircleShape; thickness: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the source texture of a circle shape

  If the shape has no source texture, a NULL pointer is returned.
  The returned pointer is const, which means that you can't
  modify the texture when you retrieve it with this function.

  \param shape Shape object

  \return Pointer to the shape's texture
*)
function sfCircleShape_getTexture(const shape: PsfCircleShape): PsfTexture; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the sub-rectangle of the texture displayed by a circle shape

  \param shape Shape object

  \return Texture rectangle of the shape
*)
function sfCircleShape_getTextureRect(const shape: PsfCircleShape): sfIntRect; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the fill color of a circle shape

  \param shape Shape object

  \return Fill color of the shape
*)
function sfCircleShape_getFillColor(const shape: PsfCircleShape): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the outline color of a circle shape

  \param shape Shape object

  \return Outline color of the shape
*)
function sfCircleShape_getOutlineColor(const shape: PsfCircleShape): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the outline thickness of a circle shape

  \param shape Shape object

  \return Outline thickness of the shape
*)
function sfCircleShape_getOutlineThickness(const shape: PsfCircleShape): cfloat; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the total number of points of a circle shape

  \param shape Shape object

  \return Number of points of the shape
*)
function sfCircleShape_getPointCount(const shape: PsfCircleShape): csize_t; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get a point of a circle shape

  The result is undefined if \a index is out of the valid range.

  \param shape Shape object
  \param index Index of the point to get, in range [0 .. getPointCount() - 1]

  \return Index-th point of the shape
*)
function sfCircleShape_getPoint(const shape: PsfCircleShape; index: csize_t): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the radius of a circle

  \param shape  Shape object
  \param radius New radius of the circle
*)
procedure sfCircleShape_setRadius(shape: PsfCircleShape; radius: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the radius of a circle

  \param shape Shape object

  \return Radius of the circle
*)
function sfCircleShape_getRadius(const shape: PsfCircleShape): cfloat; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the number of points of a circle

  \param shape Shape object
  \param count New number of points of the circle
*)
procedure sfCircleShape_setPointCount(shape: PsfCircleShape; count: csize_t); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the local bounding rectangle of a circle shape

  The returned rectangle is in local coordinates, which means
  that it ignores the transformations (translation, rotation,
  scale, ...) that are applied to the entity.
  In other words, this function returns the bounds of the
  entity in the entity's coordinate system.

  \param shape Shape object

  \return Local bounding rectangle of the entity
*)
function sfCircleShape_getLocalBounds(const shape: PsfCircleShape): sfFloatRect; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the global bounding rectangle of a circle shape

  The returned rectangle is in global coordinates, which means
  that it takes in account the transformations (translation,
  rotation, scale, ...) that are applied to the entity.
  In other words, this function returns the bounds of the
  sprite in the global 2D world's coordinate system.

  \param shape Shape object

  \return Global bounding rectangle of the entity
*)
function sfCircleShape_getGlobalBounds(const shape: PsfCircleShape): sfFloatRect; cdecl; external LIB_CSFMLGRAPHICS;

//#endif // SFML_CIRCLESHAPE_H
