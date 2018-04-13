program SoundCapture;

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

var
  SampleRate: cint;
  Recorder: PsfSoundBufferRecorder;
  Buffer: PsfSoundBuffer;
  Choice: Char;
  FileName: string;
  Sound: PsfSound;

begin
  // Check that the device can capture audio
  if not (sfSoundRecorder_isAvailable() = sfTrue) then
  begin
    WriteLn('Sorry, audio capture is not supported by your system');
    Halt;
  end;

  // Choose the sample rate
  Write('Please choose the sample rate for sound capture (44100 is CD quality): ');
  ReadLn(SampleRate);

  // Wait for user input...
  WriteLn('Press enter to start recording audio');
  ReadLn;

  // Here we'll use an integrated custom recorder, which saves the captured data into a SoundBuffer
  Recorder := sfSoundBufferRecorder_create();
  
  // Audio capture is done in a separate thread, so we can block the main thread while it is capturing
  if (sfSoundBufferRecorder_start(Recorder, SampleRate) = sfFalse) then
  begin
    WriteLn('Recording failed!');
    sfSoundBufferRecorder_destroy(Recorder);
    Halt;
  end;
  WriteLn('Recording... press enter to stop');
  ReadLn;
  sfSoundBufferRecorder_stop(Recorder);

  // Get the buffer containing the captured data
  Buffer := sfSoundBufferRecorder_getBuffer(Recorder);

  // Display captured sound informations
  WriteLn('Sound information:');
  WriteLn(' ', sfTime_asSeconds(sfSoundBuffer_getDuration(Buffer)), ' seconds');
  WriteLn(' ', sfSoundBuffer_getSampleRate(Buffer), ' samples / seconds');
  WriteLn(' ', sfSoundBuffer_getChannelCount(Buffer), ' channels');

  // Choose what to do with the recorded sound data
  Write('What do you want to do with captured sound (p = play, s = save)? ');
  ReadLn(Choice);

  if Choice = 's' then
  begin
    // Choose the filename
    WriteLn('Choose the file to create:');
    ReadLn(FileName);

    // Save the buffer
    sfSoundBuffer_saveToFile(Buffer, PChar(FileName));
  end
  else
  begin
    // Create a sound instance and play it
    Sound := sfSound_create();
    sfSound_setBuffer(Sound, Buffer);
    sfSound_play(Sound);

    // Wait until finished
    while sfSound_getStatus(Sound) = sfPlaying do
    begin
      // Display the playing position
      WriteLn('Playing... ', sfTime_asSeconds(sfSound_getPlayingOffset(Sound)), ' sec');

      // Leave some CPU time for other threads
      sfSleep(sfMilliseconds(100));
    end;
	
	sfSound_destroy(Sound);
  end;

  // Finished!
  WriteLn('Done!');

  sfSoundBufferRecorder_destroy(Recorder);

  // Wait until the user presses 'enter' key
  Write('Press enter to exit...');
  ReadLn;
end.
