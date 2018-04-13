program Pong;

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

////////////////////////////////////////////////////////////
/// Launch a server, wait for an incoming connection,
/// send a message and wait for the answer.
///
////////////////////////////////////////////////////////////
procedure runTcpServer(port: cushort);
var
  listener: PsfTcpListener;
  socket: PsfTcpSocket;
  outstr: string;
  instr: string;
  received: csize_t;
begin
  // Create a server socket to accept new connections
  listener := sfTcpListener_create();

  // Listen to the given port for incoming connections
  if (sfTcpListener_listen(listener, port, sfIpAddress_Any) <> sfSocketDone) then exit;
  WriteLn('Server is listening to port ', port, ', waiting for connections... ');

  // Wait for a connection
  socket:= sfTcpSocket_create();
  if (sfTcpListener_accept(listener, @socket) <> sfSocketDone) then exit;
  WriteLn('Client connected: ', sfTcpSocket_getRemoteAddress(socket).address);

  // Send a message to the connected client
  outstr := 'Hi, I''m the server';
  if (sfTcpSocket_send(socket, PChar(outstr), length(outstr)) <> sfSocketDone) then exit;
  WriteLn('Message sent to the client: "', outstr, '"');

  // Receive a message back from the client
  SetLength(instr, 128);

  if (sfTcpSocket_receive(socket, PChar(instr), 128, @received) <> sfSocketDone) then exit;
  SetLength(instr, received);
  WriteLn('Answer received from the client: "', instr, '"');
end;

////////////////////////////////////////////////////////////
/// Create a client, connect it to a server, display the
/// welcome message and send an answer.
///
////////////////////////////////////////////////////////////
procedure runTcpClient(port: cushort);
var
  server: sfIpAddress;
  socket: PsfTcpSocket ;
  instr: string;
  received: csize_t;
  outstr: string;
begin
  // Ask for the server address
  repeat
    Write('Type the address or name of the server to connect to [127.0.0.1]: ');
    ReadLn(server.address);
	if (server.address = '') then server.address := '127.0.0.1';
  until (server.address <> sfIpAddress_None.address);

  // Create a socket for communicating with the server
  socket := sfTcpSocket_create();

  // Connect to the server
  if (sfTcpSocket_connect(socket, server, port, sfTime_Zero) <> sfSocketDone) then exit;
  WriteLn('Connected to server ', server.address);

  // Receive a message from the server
  SetLength(instr, 128);

  if (sfTcpSocket_receive(socket, PChar(instr), 128, @received) <> sfSocketDone) then exit;
  SetLength(instr, received);
  WriteLn('Message received from the server: "', instr, '"');

  // Send an answer to the server
  outstr := 'Hi, I''m a client';
  if (sfTcpSocket_send(socket, PChar(outstr), length(outstr)) <> sfSocketDone) then exit;
  WriteLn('Message sent to the server: "', outstr, '"');
end;

////////////////////////////////////////////////////////////
/// Launch a server, wait for a message, send an answer.
///
////////////////////////////////////////////////////////////
procedure runUdpServer(port: cushort);
var
  socket: PsfUdpSocket;
  instr: string;
  received: csize_t;
  sender: sfIpAddress;
  senderPort: cushort;
  outstr: string;
begin
  // Create a socket to receive a message from anyone
  socket := sfUdpSocket_create();

  // Listen to messages on the specified port
  if (sfUdpSocket_bind(socket, port, sfIpAddress_Any) <> sfSocketDone) then exit;
  WriteLn('Server is listening to port ', port, ', waiting for a message...');

  // Wait for a message
  SetLength(instr, 128);

  if (sfUdpSocket_receive(socket, PChar(instr), 128, @received, @sender, @senderPort) <> sfSocketDone) then exit;
  SetLength(instr, received);
  WriteLn('Message received from client ', sender.address, ': "', instr, '"');

  // Send an answer to the client
  outstr := 'Hi, I''m the server';
  if (sfUdpSocket_send(socket, PChar(outstr), length(outstr), sender, senderPort) <> sfSocketDone) then exit;
  WriteLn('Message sent to the client: "', outstr, '"');
end;

////////////////////////////////////////////////////////////
/// Send a message to the server, wait for the answer
///
////////////////////////////////////////////////////////////
procedure runUdpClient(port: cushort);
var
  server: sfIpAddress;
  socket: PsfUdpSocket;
  outstr: string;
  instr: string;
  received: csize_t;
  sender: sfIpAddress;
  senderPort: cushort;
begin
  // Ask for the server address
  repeat
    Write('Type the address or name of the server to connect to [127.0.0.1]: ');
    ReadLn(server.address);
	if (server.address = '') then server.address := '127.0.0.1';
  until (server.address <> sfIpAddress_None.address);

  // Create a socket for communicating with the server
  socket := sfUdpSocket_create();

  // Send a message to the server
  outstr := 'Hi, I''m a client';
  if (sfUdpSocket_send(socket, PChar(outstr), length(outstr), server, port) <> sfSocketDone) then exit;
  WriteLn('Message sent to the server: "', outstr, '"');

  // Receive an answer from anyone (but most likely from the server)
  SetLength(instr, 128);

  if (sfUdpSocket_receive(socket, PChar(instr), 128, @received, @sender, @senderPort) <> sfSocketDone) then exit;
  SetLength(instr, received);
  WriteLn('Message received from ', sender.address, ': "', instr, '"');
end;

////////////////////////////////////////////////////////////
/// Entry point of application
///
/// \return Application exit code
///
////////////////////////////////////////////////////////////
var
  port: cushort;
  protocol: Char;
  who: Char;
begin
  // Choose an arbitrary port for opening sockets
  port := 50001;

  // TCP, UDP or connected UDP ?
  Write('Do you want to use TCP (t) or UDP (u)? ');
  ReadLn(protocol);

  // Client or server ?
  Write('Do you want to be a server (s) or a client (c)? ');
  ReadLn(who);

  if (protocol = 't') then
  begin
    // Test the TCP protocol
    if (who = 's')
    then
      runTcpServer(port)
    else
      runTcpClient(port);
  end
  else
  begin
    // Test the unconnected UDP protocol
    if (who = 's')
    then
      runUdpServer(port)
    else
      runUdpClient(port);
  end;

  // Wait until the user presses 'enter' key
  Write('Press enter to exit...');
  ReadLn();
end.
