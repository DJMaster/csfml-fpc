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

// #ifndef SFML_SHADER_H
// #define SFML_SHADER_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/Graphics/Export.h>
// #include <SFML/Graphics/Color.h>
// #include <SFML/Graphics/Transform.h>
// #include <SFML/Graphics/Types.h>
// #include <SFML/System/InputStream.h>
// #include <SFML/System/Vector2.h>
// #include <SFML/System/Vector3.h>


////////////////////////////////////////////////////////////
/// \brief Load both the vertex and fragment shaders from files
///
/// This function can load the vertex, the geometry and the fragment
/// shaders, or only one or two of them: pass NULL if you don't want to load
/// the vertex shader, the geometry shader or the fragment shader.
/// The sources must be text files containing valid shaders
/// in GLSL language. GLSL is a C-like language dedicated to
/// OpenGL shaders; you'll probably need to read a good documentation
/// for it before writing your own shaders.
///
/// \param vertexShaderFilename   Path of the vertex shader file to load, or NULL to skip this shader
/// \param geometryShaderFilename Path of the geometry shader file to load, or NULL to skip this shader
/// \param fragmentShaderFilename Path of the fragment shader file to load, or NULL to skip this shader
///
/// \return A new sfShader object, or NULL if it failed
///
////////////////////////////////////////////////////////////
function sfShader_createFromFile(const vertexShaderFilename: pchar; const geometryShaderFilename: pchar; const fragmentShaderFilename: pchar): PsfShader; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Load both the vertex and fragment shaders from source codes in memory
///
/// This function can load the vertex, the geometry and the fragment
/// shaders, or only one or two of them: pass NULL if you don't want to load
/// the vertex shader, the geometry shader or the fragment shader.
/// The sources must be valid shaders in GLSL language. GLSL is
/// a C-like language dedicated to OpenGL shaders; you'll
/// probably need to read a good documentation for it before
/// writing your own shaders.
///
/// \param vertexShader   String containing the source code of the vertex shader, or NULL to skip this shader
/// \param geometryShader String containing the source code of the geometry shader, or NULL to skip this shader
/// \param fragmentShader String containing the source code of the fragment shader, or NULL to skip this shader
///
/// \return A new sfShader object, or NULL if it failed
///
////////////////////////////////////////////////////////////
function sfShader_createFromMemory(const vertexShader: pchar; const geometryShader: pchar; const fragmentShader: pchar): PsfShader; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Load both the vertex and fragment shaders from custom streams
///
/// This function can load the vertex, the geometry and the fragment
/// shaders, or only one or two of them: pass NULL if you don't want to load
/// the vertex shader, the geometry shader or the fragment shader.
/// The source codes must be valid shaders in GLSL language.
/// GLSL is a C-like language dedicated to OpenGL shaders;
/// you'll probably need to read a good documentation for
/// it before writing your own shaders.
///
/// \param vertexShaderStream   Source stream to read the vertex shader from, or NULL to skip this shader
/// \param geometryShaderStream Source stream to read the geometry shader from, or NULL to skip this shader
/// \param fragmentShaderStream Source stream to read the fragment shader from, or NULL to skip this shader
///
/// \return A new sfShader object, or NULL if it failed
///
////////////////////////////////////////////////////////////
function sfShader_createFromStream(vertexShaderStream: PsfInputStream; geometryShaderStream: PsfInputStream; fragmentShaderStream: PsfInputStream): PsfShader; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Destroy an existing shader
///
/// \param shader Shader to delete
///
////////////////////////////////////////////////////////////
procedure sfShader_destroy(shader: PsfShader); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Change a float parameter of a shader
///
/// \a name is the name of the variable to change in the shader.
/// The corresponding parameter in the shader must be a float
/// (float GLSL type).
///
/// Example:
/// \code
/// uniform float myparam; // this is the variable in the shader
/// \endcode
/// \code
/// sfShader_setFloatParameter(shader, "myparam", 5.2f);
/// \endcode
///
/// \param shader Shader object
/// \param name   Name of the parameter in the shader
/// \param x      Value to assign
///
////////////////////////////////////////////////////////////
procedure sfShader_setFloatParameter(shader: PsfShader; const name: pchar; x: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Change a 2-components vector parameter of a shader
///
/// \a name is the name of the variable to change in the shader.
/// The corresponding parameter in the shader must be a 2x1 vector
/// (vec2 GLSL type).
///
/// Example:
/// \code
/// uniform vec2 myparam; // this is the variable in the shader
/// \endcode
/// \code
/// sfShader_setFloat2Parameter(shader, "myparam", 5.2f, 6.0f);
/// \endcode
///
/// \param shader Shader object
/// \param name   Name of the parameter in the shader
/// \param x      First component of the value to assign
/// \param y      Second component of the value to assign
///
////////////////////////////////////////////////////////////
procedure sfShader_setFloat2Parameter(shader: PsfShader; const name: pchar; x: cfloat; y: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Change a 3-components vector parameter of a shader
///
/// \a name is the name of the variable to change in the shader.
/// The corresponding parameter in the shader must be a 3x1 vector
/// (vec3 GLSL type).
///
/// Example:
/// \code
/// uniform vec3 myparam; // this is the variable in the shader
/// \endcode
/// \code
/// sfShader_setFloat3Parameter(shader, "myparam", 5.2f, 6.0f, -8.1f);
/// \endcode
///
/// \param shader Shader object
/// \param name   Name of the parameter in the shader
/// \param x      First component of the value to assign
/// \param y      Second component of the value to assign
/// \param z      Third component of the value to assign
///
////////////////////////////////////////////////////////////
procedure sfShader_setFloat3Parameter(shader: PsfShader; const name: pchar; x: cfloat; y: cfloat; z: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Change a 4-components vector parameter of a shader
///
/// \a name is the name of the variable to change in the shader.
/// The corresponding parameter in the shader must be a 4x1 vector
/// (vec4 GLSL type).
///
/// Example:
/// \code
/// uniform vec4 myparam; // this is the variable in the shader
/// \endcode
/// \code
/// sfShader_setFloat4Parameter(shader, "myparam", 5.2f, 6.0f, -8.1f, 0.4f);
/// \endcode
///
/// \param shader Shader object
/// \param name   Name of the parameter in the shader
/// \param x      First component of the value to assign
/// \param y      Second component of the value to assign
/// \param z      Third component of the value to assign
/// \param w      Fourth component of the value to assign
///
////////////////////////////////////////////////////////////
procedure sfShader_setFloat4Parameter(shader: PsfShader; const name: pchar; x: cfloat; y: cfloat; z: cfloat; w: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Change a 2-components vector parameter of a shader
///
/// \a name is the name of the variable to change in the shader.
/// The corresponding parameter in the shader must be a 2x1 vector
/// (vec2 GLSL type).
///
/// Example:
/// \code
/// uniform vec2 myparam; // this is the variable in the shader
/// \endcode
/// \code
/// sfVector2f vec = {5.2f, 6.0f};
/// sfShader_setVector2Parameter(shader, "myparam", vec);
/// \endcode
///
/// \param shader Shader object
/// \param name   Name of the parameter in the shader
/// \param vector Vector to assign
///
////////////////////////////////////////////////////////////
procedure sfShader_setVector2Parameter(shader: PsfShader; const name: pchar; vector: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Change a 3-components vector parameter of a shader
///
/// \a name is the name of the variable to change in the shader.
/// The corresponding parameter in the shader must be a 3x1 vector
/// (vec3 GLSL type).
///
/// Example:
/// \code
/// uniform vec3 myparam; // this is the variable in the shader
/// \endcode
/// \code
/// sfVector3f vec = {5.2f, 6.0f, -8.1f};
/// sfShader_setVector3Parameter(shader, "myparam", vec);
/// \endcode
///
/// \param shader Shader object
/// \param name   Name of the parameter in the shader
/// \param vector Vector to assign
///
////////////////////////////////////////////////////////////
procedure sfShader_setVector3Parameter(shader: PsfShader; const name: pchar; vector: sfVector3f); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Change a color parameter of a shader
///
/// \a name is the name of the variable to change in the shader.
/// The corresponding parameter in the shader must be a 4x1 vector
/// (vec4 GLSL type).
///
/// It is important to note that the components of the color are
/// normalized before being passed to the shader. Therefore,
/// they are converted from range [0 .. 255] to range [0 .. 1].
/// For example, a sf::Color(255, 125, 0, 255) will be transformed
/// to a vec4(1.0, 0.5, 0.0, 1.0) in the shader.
///
/// Example:
/// \code
/// uniform vec4 color; // this is the variable in the shader
/// \endcode
/// \code
/// sfShader_setColorParameter(shader, "color", sfColor_fromRGB(255, 128, 0));
/// \endcode
///
/// \param shader Shader object
/// \param name   Name of the parameter in the shader
/// \param color  Color to assign
///
////////////////////////////////////////////////////////////
procedure sfShader_setColorParameter(shader: PsfShader; const name: pchar; color: sfColor); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Change a matrix parameter of a shader
///
/// \a name is the name of the variable to change in the shader.
/// The corresponding parameter in the shader must be a 4x4 matrix
/// (mat4 GLSL type).
///
/// Example:
/// \code
/// uniform mat4 matrix; // this is the variable in the shader
/// \endcode
/// \code
/// @todo
/// sfShader_setTransformParameter(shader, "matrix", transform);
/// \endcode
///
/// \param shader    Shader object
/// \param name      Name of the parameter in the shader
/// \param transform Transform to assign
///
////////////////////////////////////////////////////////////
procedure sfShader_setTransformParameter(shader: PsfShader; const name: pchar; transform: sfTransform); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Change a texture parameter of a shader
///
/// \a name is the name of the variable to change in the shader.
/// The corresponding parameter in the shader must be a 2D texture
/// (sampler2D GLSL type).
///
/// Example:
/// \code
/// uniform sampler2D the_texture; // this is the variable in the shader
/// \endcode
/// \code
/// sf::Texture texture;
/// ...
/// sfShader_setTextureParameter(shader, "the_texture", texture);
/// \endcode
/// It is important to note that \a texture must remain alive as long
/// as the shader uses it, no copy is made internally.
///
/// To use the texture of the object being draw, which cannot be
/// known in advance, you can use the special function
/// sfShader_setCurrentTextureParameter:
/// \code
/// sfShader_setCurrentTextureParameter(shader, "the_texture").
/// \endcode
///
/// \param shader  Shader object
/// \param name    Name of the texture in the shader
/// \param texture Texture to assign
///
////////////////////////////////////////////////////////////
procedure sfShader_setTextureParameter(shader: PsfShader; const name: pchar; const texture: PsfTexture); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Change a texture parameter of a shader
///
/// This function maps a shader texture variable to the
/// texture of the object being drawn, which cannot be
/// known in advance.
/// The corresponding parameter in the shader must be a 2D texture
/// (sampler2D GLSL type).
///
/// Example:
/// \code
/// uniform sampler2D current; // this is the variable in the shader
/// \endcode
/// \code
/// sfShader_setCurrentTextureParameter(shader, "current");
/// \endcode
///
/// \param shader Shader object
/// \param name   Name of the texture in the shader
///
////////////////////////////////////////////////////////////
procedure sfShader_setCurrentTextureParameter(shader: PsfShader; const name: pchar); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Get the underlying OpenGL handle of the shader.
///
/// You shouldn't need to use this function, unless you have
/// very specific stuff to implement that SFML doesn't support,
/// or implement a temporary workaround until a bug is fixed.
///
/// \param shader Shader object
///
/// \return OpenGL handle of the shader or 0 if not yet loaded
///
////////////////////////////////////////////////////////////
function sfShader_getNativeHandle(const shader: PsfShader): cuint; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Bind a shader for rendering (activate it)
///
/// This function is not part of the graphics API, it mustn't be
/// used when drawing SFML entities. It must be used only if you
/// mix sfShader with OpenGL code.
///
/// \code
/// sfShader *s1, *s2;
/// ...
/// sfShader_bind(s1);
/// // draw OpenGL stuff that use s1...
/// sfShader_bind(s2);
/// // draw OpenGL stuff that use s2...
/// sfShader_bind(0);
/// // draw OpenGL stuff that use no shader...
/// \endcode
///
/// \param shader Shader to bind, can be null to use no shader
///
////////////////////////////////////////////////////////////
procedure sfShader_bind(const shader: PsfShader); cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Tell whether or not the system supports shaders
///
/// This function should always be called before using
/// the shader features. If it returns false, then
/// any attempt to use sfShader will fail.
///
/// \return sfTrue if the system can use shaders, sfFalse otherwise
///
////////////////////////////////////////////////////////////
function sfShader_isAvailable(): sfBool; cdecl; external LIB_CSFMLGRAPHICS;

////////////////////////////////////////////////////////////
/// \brief Tell whether or not the system supports geometry shaders
///
/// This function should always be called before using
/// the geometry shader features. If it returns false, then
/// any attempt to use sfShader geometry shader features will fail.
///
/// This function can only return true if isAvailable() would also
/// return true, since shaders in general have to be supported in
/// order for geometry shaders to be supported as well.
///
/// Note: The first call to this function, whether by your
/// code or SFML will result in a context switch.
///
/// \return True if geometry shaders are supported, false otherwise
///
////////////////////////////////////////////////////////////
function sfShader_isGeometryAvailable(): sfBool; cdecl; external LIB_CSFMLGRAPHICS;


// #endif // SFML_SHADER_H