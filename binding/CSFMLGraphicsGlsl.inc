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

// #ifndef SFML_GLSL_H
// #define SFML_GLSL_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////  
// #include <SFML/Graphics/Transform.h>
// #include <SFML/Graphics/Color.h>
// #include <SFML/System/Vector2.h>
// #include <SFML/System/Vector3.h>

type
// 2D vectors
  PsfGlslVec2 = ^sfGlslVec2;
  sfGlslVec2 = sfVector2f;

  PsfGlslIvec2 = ^sfGlslIvec2;
  sfGlslIvec2 = sfVector2i;

  PsfGlslBvec2 = ^sfGlslBvec2;
  sfGlslBvec2 = record
    x: sfBool;
    y: sfBool;
  end;

// 3D vectors
  PsfGlslVec3 = ^sfGlslVec3;
  sfGlslVec3 = sfVector3f;

  PsfGlslIvec3 = ^sfGlslIvec3;
  sfGlslIvec3 = record
    x: cint;
    y: cint;
    z: cint;
  end;

  PsfGlslBvec3 = ^sfGlslBvec3;
  sfGlslBvec3 = record
    x: sfBool;
    y: sfBool;
    z: sfBool;
  end;

// 4D vectors
  PsfGlslVec4 = ^sfGlslVec4;
  sfGlslVec4 = record
    x: cfloat;
    y: cfloat;
    z: cfloat;
    w: cfloat;
  end;

  PsfGlslIvec4 = ^sfGlslIvec4;
  sfGlslIvec4 = record
    x: cint;
    y: cint;
    z: cint;
    w: cint;
  end;

  PsfGlslBvec4 = ^sfGlslBvec4;
  sfGlslBvec4 = record
    x: sfBool;
    y: sfBool;
    z: sfBool;
    w: sfBool;
  end;

// matrices
  PsfGlslMat3 = ^sfGlslMat3;
  sfGlslMat3 = record
    elements: array[0..3*3-1] of cfloat;
  end;

  PsfGlslMat4 = ^sfGlslMat4;
  sfGlslMat4 = record
    elements: array[0..4*4-1] of cfloat;
  end;

// #endif // SFML_GLSL_H
