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

// #ifndef SFML_BLENDMODE_H
// #define SFML_BLENDMODE_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include </Graphics/Export.h>

(*
  \brief Enumeration of the blending factors
*)
type
  sfBlendFactor = (
    sfBlendFactorZero,             ///< (0, 0, 0, 0)
    sfBlendFactorOne,              ///< (1, 1, 1, 1)
    sfBlendFactorSrcColor,         ///< (src.r, src.g, src.b, src.a)
    sfBlendFactorOneMinusSrcColor, ///< (1, 1, 1, 1) - (src.r, src.g, src.b, src.a)
    sfBlendFactorDstColor,         ///< (dst.r, dst.g, dst.b, dst.a)
    sfBlendFactorOneMinusDstColor, ///< (1, 1, 1, 1) - (dst.r, dst.g, dst.b, dst.a)
    sfBlendFactorSrcAlpha,         ///< (src.a, src.a, src.a, src.a)
    sfBlendFactorOneMinusSrcAlpha, ///< (1, 1, 1, 1) - (src.a, src.a, src.a, src.a)
    sfBlendFactorDstAlpha,         ///< (dst.a, dst.a, dst.a, dst.a)
    sfBlendFactorOneMinusDstAlpha  ///< (1, 1, 1, 1) - (dst.a, dst.a, dst.a, dst.a)
  );

(*
  \brief Enumeration of the blending equations
*)
type
  sfBlendEquation = (
    sfBlendEquationAdd,            ///< Pixel = Src * SrcFactor + Dst * DstFactor
    sfBlendEquationSubtract,       ///< Pixel = Src * SrcFactor - Dst * DstFactor
    sfBlendEquationReverseSubtract ///< Pixel = Dst * DstFactor - Src * SrcFactor
  );

(*
  \brief Blending mode for drawing
*)
type
  sfBlendMode = record
    colorSrcFactor: sfBlendFactor;  ///< Source blending factor for the color channels
    colorDstFactor: sfBlendFactor;  ///< Destination blending factor for the color channels
    colorEquation: sfBlendEquation; ///< Blending equation for the color channels
    alphaSrcFactor: sfBlendFactor;  ///< Source blending factor for the alpha channel
    alphaDstFactor: sfBlendFactor;  ///< Destination blending factor for the alpha channel
    alphaEquation: sfBlendEquation; ///< Blending equation for the alpha channel
  end;

var
  sfBlendAlpha: sfBlendMode; external LIB_CSFMLGRAPHICS;    ///< Blend source and dest according to dest alpha
  sfBlendAdd: sfBlendMode; external LIB_CSFMLGRAPHICS;      ///< Add source to dest
  sfBlendMultiply: sfBlendMode; external LIB_CSFMLGRAPHICS; ///< Multiply source and dest
  sfBlendNone: sfBlendMode; external LIB_CSFMLGRAPHICS;     ///< Overwrite dest with source

// #endif // SFML_BLENDMODE_H
