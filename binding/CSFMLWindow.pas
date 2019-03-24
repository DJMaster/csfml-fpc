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

unit CSFMLWindow;

{$mode objfpc}{$H+}

interface

uses
  ctypes,
  CSFMLConfig,
  CSFMLSystem;

const
{$ifdef WINDOWS}
  LIB_CSFMLWINDOW = 'csfml-window-2.dll';
{$endif}
{$ifdef LINUX}
  LIB_CSFMLWINDOW = 'libcsfml-window.so';
{$endif}

// #ifndef SFML_SFML_WINDOW_H
// #define SFML_SFML_WINDOW_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/System.h>
// #include <SFML/Window/Clipboard.h>
// #include <SFML/Window/Context.h>
// #include <SFML/Window/Cursor.h>
// #include <SFML/Window/Event.h>
// #include <SFML/Window/Joystick.h>
// #include <SFML/Window/JoystickIdentification.h>
// #include <SFML/Window/Keyboard.h>
// #include <SFML/Window/Mouse.h>
// #include <SFML/Window/Sensor.h>
// #include <SFML/Window/Touch.h>
// #include <SFML/Window/VideoMode.h>
// #include <SFML/Window/Window.h>

{$include CSFMLWindowTypes.inc}
{$include CSFMLWindowJoystickIdentification.inc}
{$include CSFMLWindowKeyboard.inc}
{$include CSFMLWindowMouse.inc}
{$include CSFMLWindowJoystick.inc}
{$include CSFMLWindowSensor.inc}
{$include CSFMLWindowWindowHandle.inc}
{$include CSFMLWindowEvent.inc}
{$include CSFMLWindowVideoMode.inc}
{$include CSFMLWindowWindow.inc}

{$include CSFMLWindowClipboard.inc}
{$include CSFMLWindowContext.inc}
{$include CSFMLWindowCursor.inc}
{$include CSFMLWindowTouch.inc}

// #endif // SFML_SFML_WINDOW_H

implementation

end.

