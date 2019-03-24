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

// #ifndef SFML_NETWORK_TYPES_H
// #define SFML_NETWORK_TYPES_H

type
  PsfFtpDirectoryResponse = ^sfFtpDirectoryResponse;
  sfFtpDirectoryResponse = record
  end;

  PsfFtpListingResponse = ^sfFtpListingResponse;
  sfFtpListingResponse = record
  end;

  PsfFtpResponse = ^sfFtpResponse;
  sfFtpResponse = record
  end;

  PsfFtp = ^sfFtp;
  sfFtp = record
  end;

  PsfHttpRequest = ^sfHttpRequest;
  sfHttpRequest = record
  end;

  PsfHttpResponse = ^sfHttpResponse;
  sfHttpResponse = record
  end;

  PsfHttp = ^sfHttp;
  sfHttp = record
  end;

  PsfPacket = ^sfPacket;
  sfPacket = record
  end;

  PsfSocketSelector = ^sfSocketSelector;
  sfSocketSelector = record
  end;

  PsfTcpListener = ^sfTcpListener;
  sfTcpListener = record
  end;

  PPsfTcpSocket = ^PsfTcpSocket;
  PsfTcpSocket = ^sfTcpSocket;
  sfTcpSocket = record
  end;

  PsfUdpSocket = ^sfUdpSocket;
  sfUdpSocket = record
  end;

// #endif // SFML_NETWORK_TYPES_H
