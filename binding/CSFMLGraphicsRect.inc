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

// #ifndef SFML_RECT_H
// #define SFML_RECT_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include </Graphics/Export.h>

(*
  sfFloatRect and sfIntRect are utility classes for
  manipulating rectangles.
*)
type
  PsfFloatRect = ^sfFloatRect;
  sfFloatRect = record
    left: cfloat;
    top: cfloat;
    width: cfloat;
    height: cfloat;
  end;

  PsfIntRect = ^sfIntRect;
  sfIntRect = record
    left: cint;
    top: cint;
    width: cint;
    height: cint;
  end;

(*
  \brief Check if a point is inside a rectangle's area

  \param rect Rectangle to test
  \param x    X coordinate of the point to test
  \param y    Y coordinate of the point to test

  \return sfTrue if the point is inside
*)
function sfFloatRect_contains(const rect: PsfFloatRect; x: cfloat; y: cfloat): sfBool; cdecl; external LIB_CSFMLGRAPHICS;
function sfIntRect_contains(const rect: PsfIntRect; x: cint; y: cint): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Check intersection between two rectangles

  \param rect1        First rectangle to test
  \param rect2        Second rectangle to test
  \param intersection Rectangle to be filled with overlapping rect (can be NULL)

  \return sfTrue if rectangles overlap
*)
function sfFloatRect_intersects(const rect1: PsfFloatRect; const rect2: PsfFloatRect; intersection: PsfFloatRect): sfBool; cdecl; external LIB_CSFMLGRAPHICS;
function sfIntRect_intersects(const rect1: PsfIntRect; const rect2: PsfIntRect; intersection: PsfIntRect): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

// #endif // SFML_RECT_H
