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

unit CSFMLGraphics;

{$mode objfpc}{$H+}

interface

uses
  ctypes,
  CSFMLConfig,
  CSFMLSystem,
  CSFMLWindow;

const
{$ifdef WINDOWS}
  LIB_CSFMLGRAPHICS = 'csfml-graphics-2.dll';
{$endif}
{$ifdef LINUX}
  LIB_CSFMLGRAPHICS = 'libcsfml-graphics.so';
{$endif}

// #ifndef SFML_GRAPHICS_H
// #define SFML_GRAPHICS_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////

// #include <SFML/Window.h>
// #include <SFML/Graphics/BlendMode.h>
// #include <SFML/Graphics/CircleShape.h>
// #include <SFML/Graphics/Color.h>
// #include <SFML/Graphics/ConvexShape.h>
// #include <SFML/Graphics/Font.h>
// #include <SFML/Graphics/FontInfo.h>
// #include <SFML/Graphics/Glsl.h>
// #include <SFML/Graphics/Glyph.h>
// #include <SFML/Graphics/Image.h>
// #include <SFML/Graphics/PrimitiveType.h>
// #include <SFML/Graphics/Rect.h>
// #include <SFML/Graphics/RectangleShape.h>
// #include <SFML/Graphics/RenderStates.h>
// #include <SFML/Graphics/RenderTexture.h>
// #include <SFML/Graphics/RenderWindow.h>
// #include <SFML/Graphics/Shader.h>
// #include <SFML/Graphics/Shape.h>
// #include <SFML/Graphics/Sprite.h>
// #include <SFML/Graphics/Text.h>
// #include <SFML/Graphics/Texture.h>
// #include <SFML/Graphics/Transform.h>
// #include <SFML/Graphics/Transformable.h>
// #include <SFML/Graphics/Vertex.h>
// #include <SFML/Graphics/VertexArray.h>
// #include <SFML/Graphics/VertexBuffer.h>
// #include <SFML/Graphics/View.h>

{$include CSFMLGraphicsTypes.inc}
{$include CSFMLGraphicsRect.inc}
{$include CSFMLGraphicsTransform.inc}
{$include CSFMLGraphicsColor.inc}
{$include CSFMLGraphicsVertex.inc}
{$include CSFMLGraphicsFontInfo.inc}
{$include CSFMLGraphicsGlsl.inc}
{$include CSFMLGraphicsGlyph.inc}

{$include CSFMLGraphicsBlendMode.inc}
{$include CSFMLGraphicsCircleShape.inc}
{$include CSFMLGraphicsConvexShape.inc}
{$include CSFMLGraphicsFont.inc}
{$include CSFMLGraphicsImage.inc}
{$include CSFMLGraphicsPrimitiveType.inc}
{$include CSFMLGraphicsRectangleShape.inc}
{$include CSFMLGraphicsRenderStates.inc}
{$include CSFMLGraphicsRenderTexture.inc}
{$include CSFMLGraphicsRenderWindow.inc}
{$include CSFMLGraphicsShader.inc}
{$include CSFMLGraphicsShape.inc}
{$include CSFMLGraphicsSprite.inc}
{$include CSFMLGraphicsText.inc}
{$include CSFMLGraphicsTexture.inc}
{$include CSFMLGraphicsTransformable.inc}
{$include CSFMLGraphicsVertexArray.inc}
{$include CSFMLGraphicsVertexBuffer.inc}
{$include CSFMLGraphicsView.inc}

// #endif // SFML_GRAPHICS_H

implementation

end.

