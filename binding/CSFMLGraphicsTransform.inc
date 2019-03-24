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

// #ifndef SFML_TRANSFORM_H
// #define SFML_TRANSFORM_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/Graphics/Export.h>
// #include <SFML/Graphics/Rect.h>
// #include <SFML/Graphics/Types.h>
// #include <SFML/System/Vector2.h>

(*
  \brief Encapsulate a 3x3 transform matrix
*)
type
  PsfTransform = ^sfTransform;
  sfTransform = record
    matrix: array[0..8] of cfloat;
  end;

(*
  \brief Identity transform (does nothing)
*)
var
  sfTransform_Identity: sfTransform; external LIB_CSFMLGRAPHICS;

(*
  \brief Create a new transform from a matrix

  \param a00 Element (0, 0) of the matrix
  \param a01 Element (0, 1) of the matrix
  \param a02 Element (0, 2) of the matrix
  \param a10 Element (1, 0) of the matrix
  \param a11 Element (1, 1) of the matrix
  \param a12 Element (1, 2) of the matrix
  \param a20 Element (2, 0) of the matrix
  \param a21 Element (2, 1) of the matrix
  \param a22 Element (2, 2) of the matrix

  \return A new sfTransform object
*)
function sfTransform_fromMatrix(a00: cfloat; a01: cfloat; a02: cfloat;
                                a10: cfloat; a11: cfloat; a12: cfloat;
                                a20: cfloat; a21: cfloat; a22: cfloat): sfTransform; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Return the 4x4 matrix of a transform

  This function fills an array of 16 floats with the transform
  converted as a 4x4 matrix, which is directly compatible with
  OpenGL functions.

  \code
  sfTransform transform = ...;
  float matrix[16];
  sfTransform_getMatrix(&transform, matrix)
  glLoadMatrixf(matrix);
  \endcode

  \param transform Transform object
  \param matrix Pointer to the 16-element array to fill with the matrix
*)
procedure sfTransform_getMatrix(const transform: PsfTransform; matrix: pcfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Return the inverse of a transform

  If the inverse cannot be computed, a new identity transform
  is returned.

  \param transform Transform object
  \return The inverse matrix
*)
function sfTransform_getInverse(const transform: PsfTransform): sfTransform; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Apply a transform to a 2D point

  \param transform Transform object
  \param point     Point to transform

  \return Transformed point
*)
function sfTransform_transformPoint(const transform: PsfTransform; point: sfVector2f): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Apply a transform to a rectangle

  Since SFML doesn't provide support for oriented rectangles,
  the result of this function is always an axis-aligned
  rectangle. Which means that if the transform contains a
  rotation, the bounding rectangle of the transformed rectangle
  is returned.

  \param transform Transform object
  \param rectangle Rectangle to transform

  \return Transformed rectangle
*)
function sfTransform_transformRect(const transform: PsfTransform; rectangle: sfFloatRect): sfFloatRect; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Combine two transforms

  The result is a transform that is equivalent to applying
  \a transform followed by \a other. Mathematically, it is
  equivalent to a matrix multiplication.

  \param transform Transform object
  \param other     Transform to combine to \a transform
*)
procedure sfTransform_combine(transform: PsfTransform; const other: PsfTransform); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Combine a transform with a translation

  \param transform Transform object
  \param x         Offset to apply on X axis
  \param y         Offset to apply on Y axis
*)
procedure sfTransform_translate(transform: PsfTransform; x: cfloat; y: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Combine the current transform with a rotation

  \param transform Transform object
  \param angle     Rotation angle, in degrees
*)
procedure sfTransform_rotate(transform: PsfTransform; angle: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Combine the current transform with a rotation

  The center of rotation is provided for convenience as a second
  argument, so that you can build rotations around arbitrary points
  more easily (and efficiently) than the usual
  [translate(-center), rotate(angle), translate(center)].

  \param transform Transform object
  \param angle     Rotation angle, in degrees
  \param centerX   X coordinate of the center of rotation
  \param centerY   Y coordinate of the center of rotation
*)
procedure sfTransform_rotateWithCenter(transform: PsfTransform; angle: cfloat; centerX: cfloat; centerY: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Combine the current transform with a scaling

  \param transform Transform object
  \param scaleX    Scaling factor on the X axis
  \param scaleY    Scaling factor on the Y axis
*)
procedure sfTransform_scale(transform: PsfTransform; scaleX: cfloat; scaleY: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Combine the current transform with a scaling

  The center of scaling is provided for convenience as a second
  argument, so that you can build scaling around arbitrary points
  more easily (and efficiently) than the usual
  [translate(-center), scale(factors), translate(center)]

  \param transform Transform object
  \param scaleX    Scaling factor on X axis
  \param scaleY    Scaling factor on Y axis
  \param centerX   X coordinate of the center of scaling
  \param centerY   Y coordinate of the center of scaling
*)
procedure sfTransform_scaleWithCenter(transform: PsfTransform; scaleX: cfloat; scaleY: cfloat; centerX: cfloat; centerY: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Compare two transforms for equality

  Performs an element-wise comparison of the elements of the
  left transform with the elements of the right transform.

  \param left Left operand (the first transform)
  \param right Right operand (the second transform)

  \return true if the transforms are equal, false otherwise
*)
function sfTransform_equal(left: PsfTransform; right: PsfTransform): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

// #endif // SFML_TRANSFORM_H
