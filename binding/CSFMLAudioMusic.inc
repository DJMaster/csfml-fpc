//
// SFML/CSFML Audio.h header binding for the Free Pascal Compiler aka FPC
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

// #ifndef SFML_MUSIC_H
// #define SFML_MUSIC_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include </Audio/Export.h>
// #include </Audio/SoundStatus.h>
// #include </Audio/Types.h>
// #include </System/InputStream.h>
// #include </System/Time.h>
// #include </System/Vector3.h>
// #include <stddef.h>


////////////////////////////////////////////////////////////
/// \brief Create a new music and load it from a file
///
/// This function doesn't start playing the music (call
/// sfMusic_play to do so).
/// Here is a complete list of all the supported audio formats:
/// ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
/// w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
///
/// \param filename Path of the music file to open
///
/// \return A new sfMusic object (NULL if failed)
///
////////////////////////////////////////////////////////////
function sfMusic_createFromFile(const filename: pchar): PsfMusic; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Create a new music and load it from a file in memory
///
/// This function doesn't start playing the music (call
/// sfMusic_play to do so).
/// Here is a complete list of all the supported audio formats:
/// ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
/// w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
///
/// \param data        Pointer to the file data in memory
/// \param sizeInBytes Size of the data to load, in bytes
///
/// \return A new sfMusic object (NULL if failed)
///
////////////////////////////////////////////////////////////
function sfMusic_createFromMemory(const data: pointer; sizeInBytes: csize_t): PsfMusic; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Create a new music and load it from a custom stream
///
/// This function doesn't start playing the music (call
/// sfMusic_play to do so).
/// Here is a complete list of all the supported audio formats:
/// ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
/// w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
///
/// \param stream Source stream to read from
///
/// \return A new sfMusic object (NULL if failed)
///
////////////////////////////////////////////////////////////
function sfMusic_createFromStream(stream: PsfInputStream): PsfMusic; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Destroy a music
///
/// \param music Music to destroy
///
////////////////////////////////////////////////////////////
procedure sfMusic_destroy(music: PsfMusic); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Set whether or not a music should loop after reaching the end
///
/// If set, the music will restart from beginning after
/// reaching the end and so on, until it is stopped or
/// sfMusic_setLoop(music, sfFalse) is called.
/// The default looping state for musics is false.
///
/// \param music Music object
/// \param loop  sfTrue to play in loop, sfFalse to play once
///
////////////////////////////////////////////////////////////
procedure sfMusic_setLoop(music: PsfMusic; loop: sfBool); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Tell whether or not a music is in loop mode
///
/// \param music Music object
///
/// \return sfTrue if the music is looping, sfFalse otherwise
///
////////////////////////////////////////////////////////////
function sfMusic_getLoop(const music: PsfMusic): sfBool; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Get the total duration of a music
///
/// \param music Music object
///
/// \return Music duration
///
////////////////////////////////////////////////////////////
function sfMusic_getDuration(const music: PsfMusic): sfTime; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Start or resume playing a music
///
/// This function starts the music if it was stopped, resumes
/// it if it was paused, and restarts it from beginning if it
/// was it already playing.
/// This function uses its own thread so that it doesn't block
/// the rest of the program while the music is played.
///
/// \param music Music object
///
////////////////////////////////////////////////////////////
procedure sfMusic_play(music: PsfMusic); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Pause a music
///
/// This function pauses the music if it was playing,
/// otherwise (music already paused or stopped) it has no effect.
///
/// \param music Music object
///
////////////////////////////////////////////////////////////
procedure sfMusic_pause(music: PsfMusic); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Stop playing a music
///
/// This function stops the music if it was playing or paused,
/// and does nothing if it was already stopped.
/// It also resets the playing position (unlike sfMusic_pause).
///
/// \param music Music object
///
////////////////////////////////////////////////////////////
procedure sfMusic_stop(music: PsfMusic); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Return the number of channels of a music
///
/// 1 channel means a mono sound, 2 means stereo, etc.
///
/// \param music Music object
///
/// \return Number of channels
///
////////////////////////////////////////////////////////////
function sfMusic_getChannelCount(const music: PsfMusic): cuint; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Get the sample rate of a music
///
/// The sample rate is the number of audio samples played per
/// second. The higher, the better the quality.
///
/// \param music Music object
///
/// \return Sample rate, in number of samples per second
///
////////////////////////////////////////////////////////////
function sfMusic_getSampleRate(const music: PsfMusic): cuint; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Get the current status of a music (stopped, paused, playing)
///
/// \param music Music object
///
/// \return Current status
///
////////////////////////////////////////////////////////////
function sfMusic_getStatus(const music: PsfMusic): sfSoundStatus; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Get the current playing position of a music
///
/// \param music Music object
///
/// \return Current playing position
///
////////////////////////////////////////////////////////////
function sfMusic_getPlayingOffset(const music: PsfMusic): sfTime; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Set the pitch of a music
///
/// The pitch represents the perceived fundamental frequency
/// of a sound; thus you can make a music more acute or grave
/// by changing its pitch. A side effect of changing the pitch
/// is to modify the playing speed of the music as well.
/// The default value for the pitch is 1.
///
/// \param music Music object
/// \param pitch New pitch to apply to the music
///
////////////////////////////////////////////////////////////
procedure sfMusic_setPitch(music: PsfMusic; pitch: cfloat); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Set the volume of a music
///
/// The volume is a value between 0 (mute) and 100 (full volume).
/// The default value for the volume is 100.
///
/// \param music  Music object
/// \param volume Volume of the music
///
////////////////////////////////////////////////////////////
procedure sfMusic_setVolume(music: PsfMusic; volume: cfloat); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Set the 3D position of a music in the audio scene
///
/// Only musics with one channel (mono musics) can be
/// spatialized.
/// The default position of a music is (0, 0, 0).
///
/// \param music    Music object
/// \param position Position of the music in the scene
//
////////////////////////////////////////////////////////////
procedure sfMusic_setPosition(music: PsfMusic; position: sfVector3f); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Make a musics's position relative to the listener or absolute
///
/// Making a music relative to the listener will ensure that it will always
/// be played the same way regardless the position of the listener.
/// This can be useful for non-spatialized musics, musics that are
/// produced by the listener, or musics attached to it.
/// The default value is false (position is absolute).
///
/// \param music    Music object
/// \param relative sfTrue to set the position relative, sfFalse to set it absolute
///
////////////////////////////////////////////////////////////
procedure sfMusic_setRelativeToListener(music: PsfMusic; relative: sfBool); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Set the minimum distance of a music
///
/// The "minimum distance" of a music is the maximum
/// distance at which it is heard at its maximum volume. Further
/// than the minimum distance, it will start to fade out according
/// to its attenuation factor. A value of 0 ("inside the head
/// of the listener") is an invalid value and is forbidden.
/// The default value of the minimum distance is 1.
///
/// \param music    Music object
/// \param distance New minimum distance of the music
///
////////////////////////////////////////////////////////////
procedure sfMusic_setMinDistance(music: PsfMusic; distance: cfloat); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Set the attenuation factor of a music
///
/// The attenuation is a multiplicative factor which makes
/// the music more or less loud according to its distance
/// from the listener. An attenuation of 0 will produce a
/// non-attenuated music, i.e. its volume will always be the same
/// whether it is heard from near or from far. On the other hand,
/// an attenuation value such as 100 will make the music fade out
/// very quickly as it gets further from the listener.
/// The default value of the attenuation is 1.
///
/// \param music       Music object
/// \param attenuation New attenuation factor of the music
///
////////////////////////////////////////////////////////////
procedure sfMusic_setAttenuation(music: PsfMusic; attenuation: cfloat); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Change the current playing position of a music
///
/// The playing position can be changed when the music is
/// either paused or playing.
///
/// \param music      Music object
/// \param timeOffset New playing position
///
////////////////////////////////////////////////////////////
procedure sfMusic_setPlayingOffset(music: PsfMusic; timeOffset: sfTime); cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Get the pitch of a music
///
/// \param music Music object
///
/// \return Pitch of the music
///
////////////////////////////////////////////////////////////
function sfMusic_getPitch(const music: PsfMusic): cfloat; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Get the volume of a music
///
/// \param music Music object
///
/// \return Volume of the music, in the range [0, 100]
///
////////////////////////////////////////////////////////////
function sfMusic_getVolume(const music: PsfMusic): cfloat; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Get the 3D position of a music in the audio scene
///
/// \param music Music object
///
/// \return Position of the music in the world
///
////////////////////////////////////////////////////////////
function sfMusic_getPosition(const music: PsfMusic): sfVector3f; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Tell whether a music's position is relative to the
///        listener or is absolute
///
/// \param music Music object
///
/// \return sfTrue if the position is relative, sfFalse if it's absolute
///
////////////////////////////////////////////////////////////
function sfMusic_isRelativeToListener(const music: PsfMusic): sfBool; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Get the minimum distance of a music
///
/// \param music Music object
///
/// \return Minimum distance of the music
///
////////////////////////////////////////////////////////////
function sfMusic_getMinDistance(const music: PsfMusic): cfloat; cdecl; external LIB_CSFMLAUDIO;

////////////////////////////////////////////////////////////
/// \brief Get the attenuation factor of a music
///
/// \param music Music object
///
/// \return Attenuation factor of the music
///
////////////////////////////////////////////////////////////
function sfMusic_getAttenuation(const music: PsfMusic): cfloat; cdecl; external LIB_CSFMLAUDIO;


// #endif // SFML_MUSIC_H