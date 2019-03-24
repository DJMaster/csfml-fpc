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

// #ifndef SFML_TRANSFORMABLE_H
// #define SFML_TRANSFORMABLE_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/Graphics/Export.h>
// #include <SFML/Graphics/Types.h>
// #include <SFML/Graphics/Transform.h>
// #include <SFML/System/Vector2.h>

(*
  \brief Create a new transformable

  \return A new sfTransformable object
*)
function sfTransformable_create(): PsfTransformable; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Copy an existing transformable

  \param transformable Transformable to copy

  \return Copied object
*)
function sfTransformable_copy(const transformable: PsfTransformable ): PsfTransformable; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Destroy an existing transformable

  \param transformable Transformable to delete
*)
procedure sfTransformable_destroy(transformable: PsfTransformable); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the position of a transformable

  This function completely overwrites the previous position.
  See sfTransformable_move to apply an offset based on the previous position instead.
  The default position of a transformable Transformable object is (0, 0).

  \param transformable Transformable object
  \param position      New position
*)
procedure sfTransformable_setPosition(transformable: PsfTransformable; position: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the orientation of a transformable

  This function completely overwrites the previous rotation.
  See sfTransformable_rotate to add an angle based on the previous rotation instead.
  The default rotation of a transformable Transformable object is 0.

  \param transformable Transformable object
  \param angle         New rotation, in degrees
*)
procedure sfTransformable_setRotation(transformable: PsfTransformable; angle: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the scale factors of a transformable

  This function completely overwrites the previous scale.
  See sfTransformable_scale to add a factor based on the previous scale instead.
  The default scale of a transformable Transformable object is (1, 1).

  \param transformable Transformable object
  \param scale         New scale factors
*)
procedure sfTransformable_setScale(transformable: PsfTransformable; scale: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the local origin of a transformable

  The origin of an object defines the center point for
  all transformations (position, scale, rotation).
  The coordinates of this point must be relative to the
  top-left corner of the object, and ignore all
  transformations (position, scale, rotation).
  The default origin of a transformable Transformable object is (0, 0).

  \param transformable Transformable object
  \param origin        New origin
*)
procedure sfTransformable_setOrigin(transformable: PsfTransformable; origin: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the position of a transformable

  \param transformable Transformable object

  \return Current position
*)
function sfTransformable_getPosition(const transformable: PsfTransformable): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the orientation of a transformable

  The rotation is always in the range [0, 360].

  \param transformable Transformable object

  \return Current rotation, in degrees
*)
function sfTransformable_getRotation(const transformable: PsfTransformable): cfloat; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the current scale of a transformable

  \param transformable Transformable object

  \return Current scale factors
*)
function sfTransformable_getScale(const transformable: PsfTransformable): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the local origin of a transformable

  \param transformable Transformable object

  \return Current origin
*)
function sfTransformable_getOrigin(const transformable: PsfTransformable): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Move a transformable by a given offset

  This function adds to the current position of the object,
  unlike sfTransformable_setPosition which overwrites it.

  \param transformable Transformable object
  \param offset        Offset
*)
procedure sfTransformable_move(transformable: PsfTransformable; offset: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Rotate a transformable

  This function adds to the current rotation of the object,
  unlike sfTransformable_setRotation which overwrites it.

  \param transformable Transformable object
  \param angle         Angle of rotation, in degrees
*)
procedure sfTransformable_rotate(transformable: PsfTransformable; angle: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Scale a transformable

  This function multiplies the current scale of the object,
  unlike sfTransformable_setScale which overwrites it.

  \param transformable Transformable object
  \param factors       Scale factors
*)
procedure sfTransformable_scale(transformable: PsfTransformable; factors: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the combined transform of a transformable

  \param transformable Transformable object

  \return Transform combining the position/rotation/scale/origin of the object
*)
function sfTransformable_getTransform(const transformable: PsfTransformable): sfTransform; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the inverse of the combined transform of a transformable

  \param transformable Transformable object

  \return Inverse of the combined transformations applied to the object
*)
function sfTransformable_getInverseTransform(const transformable: PsfTransformable): sfTransform; cdecl; external LIB_CSFMLGRAPHICS;

// #endif // SFML_TRANSFORMABLE_H
