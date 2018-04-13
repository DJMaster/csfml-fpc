program Sound;

{$mode objfpc}{$H+}

uses
  SysUtils,
  ctypes,
  CSFMLConfig,
  CSFMLAudio,
  CSFMLGraphics,
  CSFMLNetwork,
  CSFMLSystem,
  CSFMLWindow;

procedure PlaySound;
var
  SoundBuffer: PsfSoundBuffer;
  Sound: PsfSound;
begin
  // Load a sound buffer from a wav file
  SoundBuffer := sfSoundBuffer_createFromFile('../resources/canary.wav');

  // Display sound informations
  WriteLn('Canary.wav:');
  WriteLn(' ', FloatToStr(sfTime_asSeconds( sfSoundBuffer_getDuration(SoundBuffer))), ' seconds');
  WriteLn(' ', IntToStr(sfSoundBuffer_getSampleRate(SoundBuffer)), ' samples / sec');
  WriteLn(' ', IntToStr(sfSoundBuffer_getChannelCount(SoundBuffer)), ' channels');

  // Create a sound instance and play it
  Sound := sfSound_create();
  sfSound_setBuffer(Sound, SoundBuffer);
  sfSound_play(Sound);

  // Loop while the sound is playing
  while sfSound_getStatus(Sound) = sfPlaying do
  begin
    // Display the playing position
    WriteLn('Playing... ', FloatToStr(sfTime_asSeconds(sfSound_getPlayingOffset(Sound))), ' sec');

    // Leave some CPU time for other processes
    sfSleep(sfMilliseconds(100));
  end;
  WriteLn;

  sfSound_destroy(Sound);
  sfSoundBuffer_destroy(SoundBuffer);
end;

procedure PlayMusic(FileName: String);
var
  Music: PsfMusic;
begin
  // Load an ogg music file
  Music := sfMusic_createFromFile(PChar('../resources/' + FileName));

  // Display music informations
  WriteLn(FileName + ':');
  WriteLn(' ', FloatToStr(sfTime_asSeconds(sfMusic_getDuration(Music))), ' seconds');
  WriteLn(' ', IntToStr(sfMusic_getSampleRate(Music)), ' samples / sec');
  WriteLn(' ', IntToStr(sfMusic_getChannelCount(Music)), ' channels');

  // Play it
  sfMusic_play(Music);

  // Loop while the music is playing
  while sfMusic_getStatus(Music) = sfPlaying do
  begin
    // Display the playing position
    WriteLn('Playing... ', FloatToStr(sfTime_asSeconds(sfMusic_getPlayingOffset(Music))), ' sec');

    // Leave some CPU time for other processes
    sfSleep(sfMilliseconds(100));
  end;
  WriteLn;

  sfMusic_destroy(Music);
end;

begin
  // Play a sound
  PlaySound;

  // Play music from an ogg file
  PlayMusic('orchestral.ogg');

  // Play music from a flac file
  PlayMusic('ding.flac');

  // Wait until the user presses 'enter' key
  Write('Press enter to exit...');
  ReadLn;
end.
