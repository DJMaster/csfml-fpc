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

unit CSFMLNetwork;

{$mode objfpc}{$H+}

interface

uses
  ctypes,
  CSFMLConfig,
  CSFMLSystem;

const
{$ifdef WINDOWS}
  LIB_CSFMLNETWORK = 'csfml-network-2.dll';
{$endif}
{$ifdef LINUX}
  LIB_CSFMLNETWORK = 'libcsfml-network.so';
{$endif}

// #ifndef SFML_NETWORK_H
// #define SFML_NETWORK_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////

// #include <SFML/System.h>
// #include <SFML/Network/Ftp.h>
// #include <SFML/Network/Http.h>
// #include <SFML/Network/IpAddress.h>
// #include <SFML/Network/Packet.h>
// #include <SFML/Network/SocketSelector.h>
// #include <SFML/Network/SocketStatus.h>
// #include <SFML/Network/TcpListener.h>
// #include <SFML/Network/TcpSocket.h>
// #include <SFML/Network/UdpSocket.h>

{$include CSFMLNetworkTypes.inc}
{$include CSFMLNetworkIpAddress.inc}

{$include CSFMLNetworkFtp.inc}
{$include CSFMLNetworkHttp.inc}
{$include CSFMLNetworkPacket.inc}
{$include CSFMLNetworkSocketSelector.inc}
{$include CSFMLNetworkSocketStatus.inc}
{$include CSFMLNetworkTcpListener.inc}
{$include CSFMLNetworkTcpSocket.inc}
{$include CSFMLNetworkUdpSocket.inc}

// #endif // SFML_NETWORK_H

implementation

end.

