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

// #ifndef SFML_COLOR_H
// #define SFML_COLOR_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include </Graphics/Export.h>

(*
  \brief Utility class for manpulating RGBA colors
*)
type
  sfColor = record
    r: sfUint8;
    g: sfUint8;
    b: sfUint8;
    a: sfUint8;
  end;

var
  sfBlack: sfColor; external LIB_CSFMLGRAPHICS;       ///< Black predefined color
  sfWhite: sfColor; external LIB_CSFMLGRAPHICS;       ///< White predefined color
  sfRed: sfColor; external LIB_CSFMLGRAPHICS;         ///< Red predefined color
  sfGreen: sfColor; external LIB_CSFMLGRAPHICS;       ///< Green predefined color
  sfBlue: sfColor; external LIB_CSFMLGRAPHICS;        ///< Blue predefined color
  sfYellow: sfColor; external LIB_CSFMLGRAPHICS;      ///< Yellow predefined color
  sfMagenta: sfColor; external LIB_CSFMLGRAPHICS;     ///< Magenta predefined color
  sfCyan: sfColor; external LIB_CSFMLGRAPHICS;        ///< Cyan predefined color
  sfTransparent: sfColor; external LIB_CSFMLGRAPHICS; ///< Transparent (black) predefined color

(*
  \brief Construct a color from its 3 RGB components

  \param red   Red component   (0 .. 255)
  \param green Green component (0 .. 255)
  \param blue  Blue component  (0 .. 255)

  \return sfColor constructed from the components
*)
function sfColor_fromRGB(red: sfUint8; green: sfUint8; blue: sfUint8): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Construct a color from its 4 RGBA components

  \param red   Red component   (0 .. 255)
  \param green Green component (0 .. 255)
  \param blue  Blue component  (0 .. 255)
  \param alpha Alpha component (0 .. 255)

  \return sfColor constructed from the components
*)
function sfColor_fromRGBA(red: sfUint8; green: sfUint8; blue: sfUint8; alpha: sfUint8): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Construct the color from 32-bit unsigned integer

  \param color Number containing the RGBA components (in that order)

  \return sfColor constructed from the 32-bit unsigned integer
*)
function sfColor_fromInteger(color: sfUint32): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Convert a color to a 32-bit unsigned integer

  \return Color represented as a 32-bit unsigned integer
*)
function sfColor_toInteger(color: sfColor): sfUint32; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Add two colors

  \param color1 First color
  \param color2 Second color

  \return Component-wise saturated addition of the two colors
*)
function sfColor_add(color1: sfColor; color2: sfColor): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Subtract two colors

  \param color1 First color
  \param color2 Second color

  \return Component-wise saturated subtraction of the two colors
*)
function sfColor_subtract(color1: sfColor; color2: sfColor): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Modulate two colors

  \param color1 First color
  \param color2 Second color

  \return Component-wise multiplication of the two colors
*)
function sfColor_modulate(color1: sfColor; color2: sfColor): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

// #endif // SFML_COLOR_H
