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

// #ifndef SFML_EVENT_H
// #define SFML_EVENT_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/Window/Export.h>
// #include <SFML/Window/Joystick.h>
// #include <SFML/Window/Keyboard.h>
// #include <SFML/Window/Mouse.h>
// #include <SFML/Window/Sensor.h>

(*
  \brief Definition of all the event types
*)
type
  sfEventType = (
    sfEvtClosed,                 ///< The window requested to be closed (no data)
    sfEvtResized,                ///< The window was resized (data in event.size)
    sfEvtLostFocus,              ///< The window lost the focus (no data)
    sfEvtGainedFocus,            ///< The window gained the focus (no data)
    sfEvtTextEntered,            ///< A character was entered (data in event.text)
    sfEvtKeyPressed,             ///< A key was pressed (data in event.key)
    sfEvtKeyReleased,            ///< A key was released (data in event.key)
    sfEvtMouseWheelMoved,        ///< The mouse wheel was scrolled (data in event.mouseWheel) (deprecated)
    sfEvtMouseWheelScrolled,     ///< The mouse wheel was scrolled (data in event.mouseWheelScroll)
    sfEvtMouseButtonPressed,     ///< A mouse button was pressed (data in event.mouseButton)
    sfEvtMouseButtonReleased,    ///< A mouse button was released (data in event.mouseButton)
    sfEvtMouseMoved,             ///< The mouse cursor moved (data in event.mouseMove)
    sfEvtMouseEntered,           ///< The mouse cursor entered the area of the window (no data)
    sfEvtMouseLeft,              ///< The mouse cursor left the area of the window (no data)
    sfEvtJoystickButtonPressed,  ///< A joystick button was pressed (data in event.joystickButton)
    sfEvtJoystickButtonReleased, ///< A joystick button was released (data in event.joystickButton)
    sfEvtJoystickMoved,          ///< The joystick moved along an axis (data in event.joystickMove)
    sfEvtJoystickConnected,      ///< A joystick was connected (data in event.joystickConnect)
    sfEvtJoystickDisconnected,   ///< A joystick was disconnected (data in event.joystickConnect)
    sfEvtTouchBegan,             ///< A touch event began (data in event.touch)
    sfEvtTouchMoved,             ///< A touch moved (data in event.touch)
    sfEvtTouchEnded,             ///< A touch event ended (data in event.touch)
    sfEvtSensorChanged,          ///< A sensor value changed (data in event.sensor)

    sfEvtCount                   ///< Keep last -- the total number of event types
  );

(*
  \brief Keyboard event parameters
*)
type
  sfKeyEvent = record
    type_: sfEventType;
    code: sfKeyCode;
    alt: sfBool;
    control: sfBool;
    shift: sfBool;
    system: sfBool;
  end;

(*
  \brief Text event parameters
*)
type
  sfTextEvent = record
    type_: sfEventType;
    unicode: sfUint32;
  end;

(*
  \brief Mouse move event parameters
*)
type
  sfMouseMoveEvent = record
    type_: sfEventType;
    x: cint;
    y: cint;
  end;

(*
  \brief Mouse buttons events parameters
*)
type
  sfMouseButtonEvent = record
    type_: sfEventType;
    button: sfMouseButton;
    x: cint;
    y: cint;
  end;

(*
  \brief Mouse wheel events parameters

  \deprecated
  Use sfMouseWheelScrollEvent instead.
*)
type
  sfMouseWheelEvent = record
    type_: sfEventType;
    delta: cint;
    x: cint;
    y: cint;
  end;

(*
  \brief Mouse wheel events parameters
*)
type
  sfMouseWheelScrollEvent = record
    type_: sfEventType;
    wheel: sfMouseWheel;
    delta: cfloat;
    x: cint;
    y: cint;
  end;

(*
  \brief Joystick axis move event parameters
*)
type
  sfJoystickMoveEvent = record
    type_: sfEventType;
    joystickId: cuint;
    axis: sfJoystickAxis;
    position: cfloat;
  end;

(*
  \brief Joystick buttons events parameters
*)
type
  sfJoystickButtonEvent = record
    type_: sfEventType;
    joystickId: cuint;
    button: cuint;
  end;

(*
  \brief Joystick connection/disconnection event parameters
*)
type
  sfJoystickConnectEvent = record
    type_: sfEventType;
    joystickId: cuint;
  end;

(*
  \brief Size events parameters
*)
type
  sfSizeEvent = record
    type_: sfEventType;
    width: cuint;
    height: cuint;
  end;

(*
  \brief Touch events parameters
*)
type
  sfTouchEvent = record
    type_: sfEventType;
    finger: cuint;
    x: cint;
    y: cint;
  end;

(*
  \brief Sensor event parameters
*)
type
  sfSensorEvent = record
    type_: sfEventType;
    sensorType: sfSensorType;
    x: cfloat;
    y: cfloat;
    z: cfloat;
  end;

(*
  \brief sfEvent defines a system event and its parameters
*)
type
  PsfEvent = ^sfEvent;
  sfEvent = record
    case cint of
     0: (type_: sfEventType);                        ///< Type of the event
     1: (size: sfSizeEvent);                         ///< Size event parameters
     2: (key: sfKeyEvent);                           ///< Key event parameters
     3: (text: sfTextEvent);                         ///< Text event parameters
     4: (mouseMove: sfMouseMoveEvent);               ///< Mouse move event parameters
     5: (mouseButton: sfMouseButtonEvent);           ///< Mouse button event parameters
     6: (mouseWheel: sfMouseWheelEvent);             ///< Mouse wheel event parameters (deprecated)
     7: (mouseWheelScroll: sfMouseWheelScrollEvent); ///< Mouse wheel event parameters
     8: (joystickMove: sfJoystickMoveEvent);         ///< Joystick move event parameters
     9: (joystickButton: sfJoystickButtonEvent);     ///< Joystick button event parameters
    10: (joystickConnect: sfJoystickConnectEvent);   ///< Joystick (dis)connect event parameters
    11: (touch: sfTouchEvent);                       ///< Touch events parameters
    12: (sensor: sfSensorEvent);                     ///< Sensor event parameters
  end;

// #endif // SFML_EVENT_H
