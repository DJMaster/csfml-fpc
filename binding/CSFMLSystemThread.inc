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

// #ifndef SFML_THREAD_H
// #define SFML_THREAD_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/System/Export.h>
// #include <SFML/System/Types.h>

(*
  \brief Create a new thread from a function pointer

  Note: this does *not* run the thread, use sfThread_launch.

  \param function Entry point of the thread
  \param userData Custom data to pass to the thread function

  \return A new sfThread object
*)
type
  sfThreadEntryPoint = procedure(UserData: Pointer); cdecl;

function sfThread_create(proc: sfThreadEntryPoint; userData: pointer): PsfThread; cdecl; external LIB_CSFMLSYSTEM;

(*
  \brief Destroy a thread

  This function calls sfThread_wait, so that the internal thread
  cannot survive after the sfThread object is destroyed.

  \param thread Thread to destroy
*)
procedure sfThread_destroy(thread: PsfThread); cdecl; external LIB_CSFMLSYSTEM;

(*
  \brief Run a thread

  This function starts the entry point passed to the
  thread's constructor, and returns immediately.
  After this function returns, the thread's function is
  running in parallel to the calling code.

  \param thread Thread object
*)
procedure sfThread_launch(thread: PsfThread); cdecl; external LIB_CSFMLSYSTEM;

(*
  \brief Wait until a thread finishes

  This function will block the execution until the
  thread's function ends.
  Warning: if the thread function never ends, the calling
  thread will block forever.
  If this function is called from its owner thread, it
  returns without doing anything.

  \param thread Thread object
*)
procedure sfThread_wait(thread: PsfThread); cdecl; external LIB_CSFMLSYSTEM;

(*
  \brief Terminate a thread

  This function immediately stops the thread, without waiting
  for its function to finish.
  Terminating a thread with this function is not safe,
  and can lead to local variables not being destroyed
  on some operating systems. You should rather try to make
  the thread function terminate by itself.

  \param thread Thread object
*)
procedure sfThread_terminate(thread: PsfThread); cdecl; external LIB_CSFMLSYSTEM;

// #endif // SFML_THREAD_H
