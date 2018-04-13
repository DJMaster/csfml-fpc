program Voip;

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

const
  Port = 2435;
  SampleRate = 44100;

const
  AudioData = 1;
  EndOfStream = 2;

var
  // server
  Player: PsfSoundStream;
  Listener: PsfTcpListener;
  Client: PsfTcpSocket;
  Mutex: PsfMutex;
  
  Samples: array of sfInt16;
  TempBuffer: array of sfInt16;
  Offset: csize_t;
  HasFinished: Boolean;
  // client
  Recorder: PsfSoundRecorder;
  Host: sfIpAddress;
  Socket: PsfTcpSocket;


  function OnGetData(Data: PsfSoundStreamChunk): sfBool; cdecl;
  begin
    // We have reached the end of the buffer and all audio data have been played: we can stop playback
    if ((Offset >= Length(Samples)) and HasFinished) then
      Exit(sfFalse);
  
    // No new data has arrived since last update: wait until we get some
    while ((Offset >= Length(Samples)) and not HasFinished) do
      sfSleep(sfMilliseconds(10));
  
    // Copy samples into a local buffer to avoid synchronization problems
    // (don't forget that we run in two separate threads)
    sfMutex_lock(Mutex);
    try
      // m_tempBuffer.assign(m_samples.begin() + m_offset, m_samples.end());
      SetLength(TempBuffer, Length(Samples) - Offset);
	  FillChar(TempBuffer[0], Length(TempBuffer) * SizeOf(SmallInt), 0);
      Move(Samples[Offset], TempBuffer[0], Length(TempBuffer) * SizeOf(SmallInt));
    finally
      sfMutex_unlock(Mutex);
    end;
  
    // Fill audio data to pass to the stream
    Data^.Samples     := @TempBuffer[0];
    Data^.SampleCount := Length(TempBuffer);
  
    // Update the playing offset
    Offset := Offset + Length(TempBuffer);
  
    Result := sfTrue;
  end;

  procedure OnSeek(TimeOffset: sfTime); cdecl;
  begin
    Offset := sfTime_asMilliseconds(TimeOffset) * SampleRate div 1000;
  end;

procedure DoServer(Port: Word);

  procedure ReceiveLoop;
  var
    Packet: PsfPacket;
    ID: Byte;
    Data: pointer;
    SampleCount, Pos: Integer;
  begin
    Packet := sfPacket_create();
    try
      while not HasFinished do
      begin
        // Get waiting audio data from the network
        if (sfTcpSocket_receivePacket(Client, Packet) <> sfSocketDone) then
          Break;
  
        // Extract the message ID
        ID := sfPacket_readUint8(Packet);
  
        if ID = AudioData then
        begin
          // Extract audio samples from the packet, and append it to our samples buffer
          Data := sfPacket_getData(Packet);
		  inc(Data);
          SampleCount := (sfPacket_getDataSize(Packet) - 1) div SizeOf(SmallInt);
  
          // Don't forget that the other thread can access the sample array at any time
          // (so we protect any operation on it with the mutex)
          sfMutex_lock(Mutex);
          try
            Pos := Length(Samples);
            SetLength(Samples, Length(Samples) + SampleCount);
            Move(Data^, Samples[Pos], SampleCount * SizeOf(SmallInt));
          finally
            sfMutex_unlock(Mutex);
          end;
        end
        else if ID = EndOfStream then
        begin
          // End of stream reached: we stop receiving audio data
          WriteLn('Audio data has been 100% received!');
          HasFinished := True;
        end
        else
        begin
          // Something's wrong...
          WriteLn('Invalid packet received...');
          HasFinished := True;
        end;
      end;
    finally
      sfPacket_destroy(Packet);
    end;
  end;

begin
  Listener := sfTcpListener_create();
  Client := sfTcpSocket_create();
  Mutex := sfMutex_create();

  Offset := 0;
  HasFinished := False;
  
  Player := sfSoundStream_create(sfSoundStreamGetDataCallback(@OnGetData), sfSoundStreamSeekCallback(@OnSeek), 1, SampleRate, nil);
  
  // Build an audio stream to play sound data as it is received through the network

  // Listen to the given port for incoming connections
  if (sfTcpListener_listen(Listener, Port, sfIpAddress_Any) <> sfSocketDone) then
    Exit;

  WriteLn('Server is listening to port ', Port, ', waiting for connections...');

  // Wait for a connection
  if (sfTcpListener_accept(Listener, @Client) <> sfSocketDone) then
    Exit;
  WriteLn('Client connected: ', sfTcpSocket_getRemoteAddress(Client).address);

  // Start playback
  sfSoundStream_play(Player);

  // Start receiving audio data
  ReceiveLoop;

  // Wait until the user presses 'enter' key
  Write('Press enter to replay the sound...');
  ReadLn;

  // Replay the sound (just to make sure replaying the received data is OK)
  sfSoundStream_play(Player);

  // Loop until the sound playback is finished
  while (sfSoundStream_getStatus(Player) <> sfStopped) do
  begin
    // Leave some CPU time for other threads
    sfSleep(sfMilliseconds(100));
  end;
  
  sfSoundStream_destroy(Player);
  sfTcpListener_destroy(Listener);
  sfTcpSocket_destroy(Client);
  sfMutex_destroy(Mutex);
end;

  function OnStart(): sfBool; cdecl;
  begin
    if sfTcpSocket_connect(Socket, Host, Port, sfTime_Zero) = sfSocketDone then
    begin
      WriteLn('Connected to server ', Host.address);
      Result := sfTrue;
    end
    else
      Result := sfFalse;
  end;

  function OnProcess(const data: PsfInt16; sampleFrames: csize_t): sfBool; cdecl;
  var
    Packet: PsfPacket;
  begin
    Result := sfFalse;
    Packet := sfPacket_create();
    try
      // Pack the audio samples into a network packet
      sfPacket_writeUint8(Packet, AudioData);
      sfPacket_append(Packet, data, sampleFrames * SizeOf(sfInt16));
  
      // Send the audio packet to the server
      if (sfTcpSocket_sendPacket(Socket, Packet) = sfSocketDone) then
	    Result := sfTrue;
    finally
      sfPacket_destroy(Packet);
    end;
  end;

  procedure OnStop; cdecl;
  var
    Packet: PsfPacket;
  begin
    Packet := sfPacket_create();
    try
      // Send a 'end-of-stream' packet
      sfPacket_writeUInt8(Packet, EndOfStream);
      sfTcpSocket_sendPacket(Socket, Packet);
  
      // Close the socket
      sfTcpSocket_disconnect(Socket);
    finally
      sfPacket_destroy(Packet);
    end;
  end;

procedure DoClient(Port: Word);
begin
  // Check that the device can capture audio
  if not (sfSoundRecorder_isAvailable() = sfTrue) then
  begin
    WriteLn('Sorry, audio capture is not supported by your system');
    Halt;
  end;

  // Ask for server address
  repeat
    Write('Type address or name of the server to connect to [127.0.0.1]: ');
    ReadLn(Host.address);
	if (Host.address = '') then Host.address := '127.0.0.1';
  until Host.address <> sfIpAddress_None.address;

  // Create an instance of our custom recorder
  Socket := sfTcpSocket_create();
  Recorder := sfSoundRecorder_create(sfSoundRecorderStartCallback(@OnStart), sfSoundRecorderProcessCallback(@OnProcess), sfSoundRecorderStopCallback(@OnStop), nil);

  // Wait for user input...
  // ReadLn;
  Write('Press enter to start recording audio');
  ReadLn;

  // Start capturing audio data
  if sfSoundRecorder_start(Recorder, SampleRate) = sfTrue then
  begin
    Write('Recording... press enter to stop');
    ReadLn;
  end
  else
    WriteLn('Recording failed!');

  sfSoundRecorder_stop(Recorder);
  sfSoundRecorder_destroy(Recorder);
  
  sfTcpSocket_destroy(Socket);
end;



var
  Who: AnsiChar;

begin
  // Client or server ?
  Write('Do you want to be a server (''s'') or a client (''c'')? ');
  ReadLn(Who);

  if Who = 's' then
  begin
    // Run as a server
    DoServer(Port);
  end
  else
  begin
    // Run as a client
    DoClient(Port);
  end;

  // Wait until the user presses 'enter' key
  Write('Press enter to exit...');
  ReadLn;
end.
