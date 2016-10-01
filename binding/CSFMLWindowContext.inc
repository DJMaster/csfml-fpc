//
// SFML/CSFML Window.h header binding for the Free Pascal Compiler aka FPC
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

// #ifndef SFML_CONTEXT_H
// #define SFML_CONTEXT_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/Window/Export.h>
// #include <SFML/Window/Types.h>


////////////////////////////////////////////////////////////
/// \brief Create a new context
///
/// This function activates the new context.
///
/// \return New sfContext object
///
////////////////////////////////////////////////////////////
function sfContext_create(): PsfContext; cdecl; external LIB_CSFMLWINDOW;

////////////////////////////////////////////////////////////
/// \brief Destroy a context
///
/// \param context Context to destroy
///
////////////////////////////////////////////////////////////
procedure sfContext_destroy(context: PsfContext); cdecl; external LIB_CSFMLWINDOW;

////////////////////////////////////////////////////////////
/// \brief Activate or deactivate explicitely a context
///
/// \param context Context object
/// \param active  sfTrue to activate, sfFalse to deactivate
///
////////////////////////////////////////////////////////////
procedure sfContext_setActive(context: PsfContext; active: sfBool); cdecl; external LIB_CSFMLWINDOW;


// #endif // SFML_CONTEXT_H