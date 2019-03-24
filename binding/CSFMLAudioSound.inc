//
// SFML/CSFML Audio.h header binding for the Free Pascal Compiler aka FPC
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

// #ifndef SFML_SOUND_H
// #define SFML_SOUND_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include </Audio/Export.h>
// #include </Audio/SoundStatus.h>
// #include </Audio/Types.h>
// #include </System/Time.h>
// #include </System/Vector3.h>

(*
  \brief Create a new sound

  \return A new sfSound object
*)
function sfSound_create(): PsfSound; cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Create a new sound by copying an existing one

  \param sound Sound to copy

  \return A new sfSound object which is a copy of \a sound
*)
function sfSound_copy(const sound: PsfSound): PsfSound; cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Destroy a sound

  \param sound Sound to destroy
*)
procedure sfSound_destroy(sound: PsfSound); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Start or resume playing a sound

  This function starts the sound if it was stopped, resumes
  it if it was paused, and restarts it from beginning if it
  was it already playing.
  This function uses its own thread so that it doesn't block
  the rest of the program while the sound is played.

  \param sound Sound object
*)
procedure sfSound_play(sound: PsfSound); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Pause a sound

  This function pauses the sound if it was playing,
  otherwise (sound already paused or stopped) it has no effect.

  \param sound Sound object
*)
procedure sfSound_pause(sound: PsfSound); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Stop playing a sound

  This function stops the sound if it was playing or paused,
  and does nothing if it was already stopped.
  It also resets the playing position (unlike sfSound_pause).

  \param sound Sound object
*)
procedure sfSound_stop(sound: PsfSound); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Set the source buffer containing the audio data to play

  It is important to note that the sound buffer is not copied,
  thus the sfSoundBuffer object must remain alive as long
  as it is attached to the sound.

  \param sound  Sound object
  \param buffer Sound buffer to attach to the sound
*)
procedure sfSound_setBuffer(sound: PsfSound; const buffer: PsfSoundBuffer); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Get the audio buffer attached to a sound

  \param sound Sound object

  \return Sound buffer attached to the sound (can be NULL)
*)
function sfSound_getBuffer(const sound: PsfSound): PsfSoundBuffer; cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Set whether or not a sound should loop after reaching the end

  If set, the sound will restart from beginning after
  reaching the end and so on, until it is stopped or
  sfSound_setLoop(sound, sfFalse) is called.
  The default looping state for sounds is false.

  \param sound Sound object
  \param loop  sfTrue to play in loop, sfFalse to play once
*)
procedure sfSound_setLoop(sound: PsfSound; loop: sfBool); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Tell whether or not a sound is in loop mode

  \param sound Sound object

  \return sfTrue if the sound is looping, sfFalse otherwise
*)
function sfSound_getLoop(const sound: PsfSound): sfBool; cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Get the current status of a sound (stopped, paused, playing)

  \param sound Sound object

  \return Current status
*)
function sfSound_getStatus(const sound: PsfSound): sfSoundStatus; cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Set the pitch of a sound

  The pitch represents the perceived fundamental frequency
  of a sound; thus you can make a sound more acute or grave
  by changing its pitch. A side effect of changing the pitch
  is to modify the playing speed of the sound as well.
  The default value for the pitch is 1.

  \param sound Sound object
  \param pitch New pitch to apply to the sound
*)
procedure sfSound_setPitch(sound: PsfSound; pitch: cfloat); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Set the volume of a sound

  The volume is a value between 0 (mute) and 100 (full volume).
  The default value for the volume is 100.

  \param sound  Sound object
  \param volume Volume of the sound
*)
procedure sfSound_setVolume(sound: PsfSound; volume: cfloat); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Set the 3D position of a sound in the audio scene

  Only sounds with one channel (mono sounds) can be
  spatialized.
  The default position of a sound is (0, 0, 0).

  \param sound    Sound object
  \param position Position of the sound in the scene
*)
procedure sfSound_setPosition(sound: PsfSound; position: sfVector3f); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Make the sound's position relative to the listener or absolute

  Making a sound relative to the listener will ensure that it will always
  be played the same way regardless the position of the listener.
  This can be useful for non-spatialized sounds, sounds that are
  produced by the listener, or sounds attached to it.
  The default value is false (position is absolute).

  \param sound    Sound object
  \param relative sfTrue to set the position relative, sfFalse to set it absolute
*)
procedure sfSound_setRelativeToListener(sound: PsfSound; relative: sfBool); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Set the minimum distance of a sound

  The "minimum distance" of a sound is the maximum
  distance at which it is heard at its maximum volume. Further
  than the minimum distance, it will start to fade out according
  to its attenuation factor. A value of 0 ("inside the head
  of the listener") is an invalid value and is forbidden.
  The default value of the minimum distance is 1.

  \param sound    Sound object
  \param distance New minimum distance of the sound
*)
procedure sfSound_setMinDistance(sound: PsfSound; distance: cfloat); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Set the attenuation factor of a sound

  The attenuation is a multiplicative factor which makes
  the sound more or less loud according to its distance
  from the listener. An attenuation of 0 will produce a
  non-attenuated sound, i.e. its volume will always be the same
  whether it is heard from near or from far. On the other hand,
  an attenuation value such as 100 will make the sound fade out
  very quickly as it gets further from the listener.
  The default value of the attenuation is 1.

  \param sound       Sound object
  \param attenuation New attenuation factor of the sound
*)
procedure sfSound_setAttenuation(sound: PsfSound; attenuation: cfloat); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Change the current playing position of a sound

  The playing position can be changed when the sound is
  either paused or playing.

  \param sound      Sound object
  \param timeOffset New playing position
*)
procedure sfSound_setPlayingOffset(sound: PsfSound; timeOffset: sfTime); cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Get the pitch of a sound

  \param sound Sound object

  \return Pitch of the sound
*)
function sfSound_getPitch(const sound: PsfSound): cfloat; cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Get the volume of a sound

  \param sound Sound object

  \return Volume of the sound, in the range [0, 100]
*)
function sfSound_getVolume(const sound: PsfSound): cfloat; cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Get the 3D position of a sound in the audio scene

  \param sound Sound object

  \return Position of the sound in the world
*)
function sfSound_getPosition(const sound: PsfSound): sfVector3f; cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Tell whether a sound's position is relative to the
         listener or is absolute

  \param sound Sound object

  \return sfTrue if the position is relative, sfFalse if it's absolute
*)
function sfSound_isRelativeToListener(const sound: PsfSound): sfBool; cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Get the minimum distance of a sound

  \param sound Sound object

  \return Minimum distance of the sound
*)
function sfSound_getMinDistance(const sound: PsfSound): cfloat; cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Get the attenuation factor of a sound

  \param sound Sound object

  \return Attenuation factor of the sound
*)
function sfSound_getAttenuation(const sound: PsfSound): cfloat; cdecl; external LIB_CSFMLAUDIO;

(*
  \brief Get the current playing position of a sound

  \param sound Sound object

  \return Current playing position
*)
function sfSound_getPlayingOffset(const sound: PsfSound): sfTime; cdecl; external LIB_CSFMLAUDIO;

// #endif // SFML_SOUND_H
