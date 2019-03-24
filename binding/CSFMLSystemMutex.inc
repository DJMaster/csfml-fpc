//
// SFML/CSFML System.h header binding for the Free Pascal Compiler aka FPC
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

// #ifndef SFML_MUTEX_H
// #define SFML_MUTEX_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/System/Export.h>
// #include <SFML/System/Types.h>

(*
  \brief Create a new mutex

  \return A new sfMutex object
*)
function sfMutex_create(): PsfMutex; cdecl; external LIB_CSFMLSYSTEM;

(*
  \brief Destroy a mutex

  \param mutex Mutex to destroy
*)
procedure sfMutex_destroy(mutex: PsfMutex); cdecl; external LIB_CSFMLSYSTEM;

(*
  \brief Lock a mutex

  \param mutex Mutex object
*)
procedure sfMutex_lock(mutex: PsfMutex); cdecl; external LIB_CSFMLSYSTEM;

(*
  \brief Unlock a mutex

  \param mutex Mutex object
*)
procedure sfMutex_unlock(mutex: PsfMutex); cdecl; external LIB_CSFMLSYSTEM;

// #endif // SFML_MUTEX_H
