//
// SFML/CSFML Graphics.h header binding for the Free Pascal Compiler aka FPC
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

// #ifndef SFML_TEXT_H
// #define SFML_TEXT_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
// #include <SFML/Graphics/Export.h>
// #include <SFML/Graphics/Color.h>
// #include <SFML/Graphics/Rect.h>
// #include <SFML/Graphics/Types.h>
// #include <SFML/Graphics/Transform.h>
// #include <SFML/System/Vector2.h>
// #include <stddef.h>

(*
  sfText styles
*)
type
  sfTextStyle = (
    sfTextRegular       = 0, ///< Regular characters, no style
    sfTextBold          = 1, ///< Bold characters
    sfTextItalic        = 2, ///< Italic characters
    sfTextUnderlined    = 4, ///< Underlined characters
    sfTextStrikeThrough = 8  ///< Strike through characters
  );

(*
  \brief Create a new text

  \return A new sfText object, or NULL if it failed
*)
function sfText_create(): PsfText; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Copy an existing text

  \param text Text to copy

  \return Copied object
*)
function sfText_copy(const text: PsfText): PsfText; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Destroy an existing text

  \param text Text to delete
*)
procedure sfText_destroy(text: PsfText); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the position of a text

  This function completely overwrites the previous position.
  See sfText_move to apply an offset based on the previous position instead.
  The default position of a text Text object is (0, 0).

  \param text     Text object
  \param position New position
*)
procedure sfText_setPosition(text: PsfText; position: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the orientation of a text

  This function completely overwrites the previous rotation.
  See sfText_rotate to add an angle based on the previous rotation instead.
  The default rotation of a text Text object is 0.

  \param text  Text object
  \param angle New rotation, in degrees
*)
procedure sfText_setRotation(text: PsfText; angle: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the scale factors of a text

  This function completely overwrites the previous scale.
  See sfText_scale to add a factor based on the previous scale instead.
  The default scale of a text Text object is (1, 1).

  \param text  Text object
  \param scale New scale factors
*)
procedure sfText_setScale(text: PsfText; scale: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the local origin of a text

  The origin of an object defines the center point for
  all transformations (position, scale, rotation).
  The coordinates of this point must be relative to the
  top-left corner of the object, and ignore all
  transformations (position, scale, rotation).
  The default origin of a text object is (0, 0).

  \param text   Text object
  \param origin New origin
*)
procedure sfText_setOrigin(text: PsfText; origin: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the position of a text

  \param text Text object

  \return Current position
*)
function sfText_getPosition(const text: PsfText): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the orientation of a text

  The rotation is always in the range [0, 360].

  \param text Text object

  \return Current rotation, in degrees
*)
function sfText_getRotation(const text: PsfText): cfloat; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the current scale of a text

  \param text Text object

  \return Current scale factors
*)
function sfText_getScale(const text: PsfText): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the local origin of a text

  \param text Text object

  \return Current origin
*)
function sfText_getOrigin(const text: PsfText): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Move a text by a given offset

  This function adds to the current position of the object,
  unlike sfText_setPosition which overwrites it.

  \param text   Text object
  \param offset Offset
*)
procedure sfText_move(text: PsfText; offset: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Rotate a text

  This function adds to the current rotation of the object,
  unlike sfText_setRotation which overwrites it.

  \param text  Text object
  \param angle Angle of rotation, in degrees
*)
procedure sfText_rotate(text: PsfText; angle: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Scale a text

  This function multiplies the current scale of the object,
  unlike sfText_setScale which overwrites it.

  \param text    Text object
  \param factors Scale factors
*)
procedure sfText_scale(text: PsfText; factors: sfVector2f); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the combined transform of a text

  \param text Text object

  \return Transform combining the position/rotation/scale/origin of the object
*)
function sfText_getTransform(const text: PsfText): sfTransform; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the inverse of the combined transform of a text

  \param text Text object

  \return Inverse of the combined transformations applied to the object
*)
function sfText_getInverseTransform(const text: PsfText): sfTransform; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the string of a text (from an ANSI string)

  A text's string is empty by default.

  \param text   Text object
  \param string New string
*)
procedure sfText_setString(text: PsfText; const string_: pchar); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the string of a text (from a unicode string)

  \param text   Text object
  \param string New string
*)
procedure sfText_setUnicodeString(text: PsfText; const string_: PsfUint32); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the font of a text

  The \a font argument refers to a texture that must
  exist as long as the text uses it. Indeed, the text
  doesn't store its own copy of the font, but rather keeps
  a pointer to the one that you passed to this function.
  If the font is destroyed and the text tries to
  use it, the behaviour is undefined.

  \param text Text object
  \param font New font
*)
procedure sfText_setFont(text: PsfText; const font: PsfFont); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the character size of a text

  The default size is 30.

  \param text Text object
  \param size New character size, in pixels
*)
procedure sfText_setCharacterSize(text: PsfText; size: cuint); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the line spacing factor

  The default spacing between lines is defined by the font.
  This method enables you to set a factor for the spacing
  between lines. By default the line spacing factor is 1.

  \param text Text object
  \param spacingFactor New line spacing factor

  \see sfText_getLineSpacing
*)
procedure sfText_setLineSpacing(text: PsfText; spacingFactor: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the letter spacing factor

  The default spacing between letters is defined by the font.
  This factor doesn't directly apply to the existing
  spacing between each character, it rather adds a fixed
  space between them which is calculated from the font
  metrics and the character size.
  Note that factors below 1 (including negative numbers) bring
  characters closer to each other.
  By default the letter spacing factor is 1.

  \param text Text object
  \param spacingFactor New letter spacing factor

  \see sfText_getLetterSpacing
*)
procedure sfText_setLetterSpacing(text: PsfText; spacingFactor: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the style of a text

  You can pass a combination of one or more styles, for
  example sfTextBold | sfTextItalic.
  The default style is sfTextRegular.

  \param text  Text object
  \param style New style
*)
procedure sfText_setStyle(text: PsfText; style: sfUint32); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the fill color of a text

  By default, the text's fill color is opaque white.
  Setting the fill color to a transparent color with an outline
  will cause the outline to be displayed in the fill area of the text.

  \param text  Text object
  \param color New fill color of the text

  \deprecated This function is deprecated and may be removed in future releases.
  Use sfText_setFillColor instead.
*)
procedure sfText_setColor(text: PsfText; color: sfColor); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the fill color of a text

  By default, the text's fill color is opaque white.
  Setting the fill color to a transparent color with an outline
  will cause the outline to be displayed in the fill area of the text.

  \param text  Text object
  \param color New fill color of the text
*)
procedure sfText_setFillColor(text: PsfText; color: sfColor); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the outline color of the text

  By default, the text's outline color is opaque black.

  \param text  Text object
  \param color New outline color of the text
*)
procedure sfText_setOutlineColor(text: PsfText; color: sfColor); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Set the thickness of the text's outline

  By default, the outline thickness is 0.

  Be aware that using a negative value for the outline
  thickness will cause distorted rendering.

  \param thickness New outline thickness, in pixels

  \see getOutlineThickness
*)
procedure sfText_setOutlineThickness(text: PsfText; thickness: cfloat); cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the string of a text (returns an ANSI string)

  \param text Text object

  \return String as a locale-dependant ANSI string
*)
function sfText_getString(const text: PsfText): pchar; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the string of a text (returns a unicode string)

  \param text Text object

  \return String as UTF-32
*)
function sfText_getUnicodeString(const text: PsfText): PsfUint32; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the font used by a text

  If the text has no font attached, a NULL pointer is returned.
  The returned pointer is const, which means that you can't
  modify the font when you retrieve it with this function.

  \param text Text object

  \return Pointer to the font
*)
function sfText_getFont(const text: PsfText): PsfFont; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the size of the characters of a text

  \param text Text object

  \return Size of the characters
*)
function sfText_getCharacterSize(const text: PsfText): cuint; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the size of the letter spacing factor

  \param text Text object

  \return Size of the letter spacing factor

  \see sfText_setLetterSpacing
*)
function sfText_getLetterSpacing(const text: PsfText): cfloat; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the size of the line spacing factor

  \param text Text object

  \return Size of the line spacing factor

  \see sfText_setLineSpacing
*)
function sfText_getLineSpacing(const text: PsfText): cfloat; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the style of a text

  \param text Text object

  \return Current string style (see sfTextStyle enum)
*)
function sfText_getStyle(const text: PsfText): sfUint32; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the fill color of a text

  \param text Text object

  \return Fill color of the text

  \deprecated This function is deprecated and may be removed in future releases.
  Use sfText_getFillColor instead.
*)
function sfText_getColor(const text: PsfText): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the fill color of a text

  \param text Text object

  \return Fill color of the text
*)
function sfText_getFillColor(const text: PsfText): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the outline color of a text

  \param text Text object

  \return Outline color of the text
*)
function sfText_getOutlineColor(const text: PsfText): sfColor; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the outline thickness of a text

  \param text Text object

  \return Outline thickness of a text, in pixels
*)
function sfText_getOutlineThickness(const text: PsfText): cfloat; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Return the position of the \a index-th character in a text

  This function computes the visual position of a character
  from its index in the string. The returned position is
  in global coordinates (translation, rotation, scale and
  origin are applied).
  If \a index is out of range, the position of the end of
  the string is returned.

  \param text  Text object
  \param index Index of the character

  \return Position of the character
*)
function sfText_findCharacterPos(const text: PsfText; index: csize_t): sfVector2f; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the local bounding rectangle of a text

  The returned rectangle is in local coordinates, which means
  that it ignores the transformations (translation, rotation,
  scale, ...) that are applied to the entity.
  In other words, this function returns the bounds of the
  entity in the entity's coordinate system.

  \param text Text object

  \return Local bounding rectangle of the entity
*)
function sfText_getLocalBounds(const text: PsfText): sfFloatRect; cdecl; external LIB_CSFMLGRAPHICS;

(*
  \brief Get the global bounding rectangle of a text

  The returned rectangle is in global coordinates, which means
  that it takes in account the transformations (translation,
  rotation, scale, ...) that are applied to the entity.
  In other words, this function returns the bounds of the
  text in the global 2D world's coordinate system.

  \param text Text object

  \return Global bounding rectangle of the entity
*)
function sfText_getGlobalBounds(const text: PsfText): sfFloatRect; cdecl; external LIB_CSFMLGRAPHICS;

// #endif // SFML_TEXT_H
