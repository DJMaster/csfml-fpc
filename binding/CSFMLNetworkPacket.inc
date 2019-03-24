//
// SFML/CSFML Network.h header binding for the Free Pascal Compiler aka FPC
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

// #ifndef SFML_PACKET_H
// #define SFML_PACKET_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/Network/Export.h>
// #include <SFML/Network/Types.h>
// #include <stddef.h>

(*
  \brief Create a new packet

  \return A new sfPacket object
*)
function sfPacket_create(): PsfPacket; cdecl; external LIB_CSFMLNETWORK;

(*
  \brief Create a new packet by copying an existing one

  \param packet Packet to copy

  \return A new sfPacket object which is a copy of \a packet
*)
function sfPacket_copy(const packet: PsfPacket): PsfPacket; cdecl; external LIB_CSFMLNETWORK;

(*
  \brief Destroy a packet

  \param packet Packet to destroy
*)
procedure sfPacket_destroy(packet: PsfPacket); cdecl; external LIB_CSFMLNETWORK;

(*
  \brief Append data to the end of a packet

  \param packet      Packet object
  \param data        Pointer to the sequence of bytes to append
  \param sizeInBytes Number of bytes to append
*)
procedure sfPacket_append(packet: PsfPacket; const data: pointer; sizeInBytes: csize_t); cdecl; external LIB_CSFMLNETWORK;

////////////////////////////////////////////////////////////
/// \brief Clear a packet
///
/// After calling Clear, the packet is empty.
///
/// \param packet Packet object
///
///////////////////////////////////////////////////////////
procedure sfPacket_clear(packet: PsfPacket); cdecl; external LIB_CSFMLNETWORK;

(*
  \brief Get a pointer to the data contained in a packet

  Warning: the returned pointer may become invalid after
  you append data to the packet, therefore it should never
  be stored.
  The return pointer is NULL if the packet is empty.

  \param packet Packet object

  \return Pointer to the data
*)
function sfPacket_getData(const packet: PsfPacket): pointer; cdecl; external LIB_CSFMLNETWORK;

(*
  \brief Get the size of the data contained in a packet

  This function returns the number of bytes pointed to by
  what sfPacket_getData returns.

  \param packet Packet object

  \return Data size, in bytes
*)
function sfPacket_getDataSize(const packet: PsfPacket): csize_t; cdecl; external LIB_CSFMLNETWORK;

(*
  \brief Tell if the reading position has reached the
         end of a packet

  This function is useful to know if there is some data
  left to be read, without actually reading it.

  \param packet Packet object

  \return sfTrue if all data was read, sfFalse otherwise
*)
function sfPacket_endOfPacket(const packet: PsfPacket): sfBool; cdecl; external LIB_CSFMLNETWORK;

(*
  \brief Test the validity of a packet, for reading

  This function allows to test the packet, to check if
  a reading operation was successful.

  A packet will be in an invalid state if it has no more
  data to read.

  \param packet Packet object

  \return sfTrue if last data extraction from packet was successful
*)
function sfPacket_canRead(const packet: PsfPacket): sfBool; cdecl; external LIB_CSFMLNETWORK;

(*
  \brief Functions to extract data from a packet

  \param packet Packet object
*)
function sfPacket_readBool(packet: PsfPacket): sfBool; cdecl; external LIB_CSFMLNETWORK;
function sfPacket_readInt8(packet: PsfPacket): sfInt8; cdecl; external LIB_CSFMLNETWORK;
function sfPacket_readUint8(packet: PsfPacket): sfUint8; cdecl; external LIB_CSFMLNETWORK;
function sfPacket_readInt16(packet: PsfPacket): sfInt16; cdecl; external LIB_CSFMLNETWORK;
function sfPacket_readUint16(packet: PsfPacket): sfUint16; cdecl; external LIB_CSFMLNETWORK;
function sfPacket_readInt32(packet: PsfPacket): sfInt32; cdecl; external LIB_CSFMLNETWORK;
function sfPacket_readUint32(packet: PsfPacket): sfUint32; cdecl; external LIB_CSFMLNETWORK;
function sfPacket_readFloat(packet: PsfPacket): cfloat; cdecl; external LIB_CSFMLNETWORK;
function sfPacket_readDouble(packet: PsfPacket): cdouble; cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_readString(packet: PsfPacket; string_: pchar); cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_readWideString(packet: PsfPacket; string_: pucs4char); cdecl; external LIB_CSFMLNETWORK;

(*
  \brief Functions to insert data into a packet

  \param packet Packet object
*)
procedure sfPacket_writeBool(packet: PsfPacket; value: sfBool); cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_writeInt8(packet: PsfPacket; value: sfInt8); cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_writeUint8(packet: PsfPacket; value: sfUint8); cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_writeInt16(packet: PsfPacket; value: sfInt16); cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_writeUint16(packet: PsfPacket; value: sfUint16); cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_writeInt32(packet: PsfPacket; value: sfInt32); cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_writeUint32(packet: PsfPacket; value: sfUint32); cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_writeFloat(packet: PsfPacket; value: cfloat); cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_writeDouble(packet: PsfPacket; value: cdouble); cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_writeString(packet: PsfPacket; const string_: pchar); cdecl; external LIB_CSFMLNETWORK;
procedure sfPacket_writeWideString(packet: PsfPacket; const string_: pucs4char); cdecl; external LIB_CSFMLNETWORK;

// #endif // SFML_PACKET_H
