//
// SFML/CSFML Window.h header binding for the Free Pascal Compiler aka FPC
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

// #ifndef SFML_CONTEXT_H
// #define SFML_CONTEXT_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/Window/Export.h>
// #include <SFML/Window/Types.h>

(*
  \brief Create a new context

  This function activates the new context.

  \return New sfContext object
*)
function sfContext_create(): PsfContext; cdecl; external LIB_CSFMLWINDOW;

(*
  \brief Destroy a context

  \param context Context to destroy
*)
procedure sfContext_destroy(context: PsfContext); cdecl; external LIB_CSFMLWINDOW;

(*
  \brief Activate or deactivate explicitely a context

  \param context Context object
  \param active  sfTrue to activate, sfFalse to deactivate

  \return sfTrue on success, sfFalse on failure
*)
function sfContext_setActive(context: PsfContext; active: sfBool): sfBool; cdecl; external LIB_CSFMLWINDOW;

(*
  \brief Get the settings of the context.

  Note that these settings may be different than the ones passed to the
  constructor; they are indeed adjusted if the original settings are not
  directly supported by the system.

  \return Structure containing the settings
*)
function sfContext_getSettings(const context: PsfContext): sfContextSettings; cdecl; external LIB_CSFMLWINDOW;

(*
  \brief Get the currently active context's ID

  The context ID is used to identify contexts when
  managing unshareable OpenGL resources.

  \return The active context's ID or 0 if no context is currently active
*)
function sfContext_getActiveContextId(): sfUint64; cdecl; external LIB_CSFMLWINDOW;

// #endif // SFML_CONTEXT_H
