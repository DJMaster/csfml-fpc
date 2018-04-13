program Ftp;

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
  Address: sfIpAddress;
  Server: PsfFtp;
  Response: PsfFtpResponse;
  DirectoryResponse: PsfFtpDirectoryResponse;
  ListingResponse: PsfFtpListingResponse;
  User, Password, FileName, Directory, Source, Destination: string;
  Index, Choice: cint;
begin
  // Choose the server address
  repeat
    Write('Enter the FTP server address [127.0.0.1]: ');
    ReadLn(Address.address);
	if (Address.address = '') then Address.address := '127.0.0.1';
  until (Address.address <> sfIpAddress_None.address);

  Server := sfFtp_create();

  // Connect to the server
  Response := sfFtp_connect(Server, Address, 21, sfTime_Zero);
  if not (sfFtpResponse_isOk(Response) = sfTrue) then exit;
  WriteLn(string(sfFtpResponse_getMessage(Response)));
  WriteLn('');

  // Ask for user name and password
  Write('User name: ');
  ReadLn(User);
  Write('Password: ');
  ReadLn(Password);

  // Login to the server
  Response := sfFtp_login(Server, PChar(User), PChar(Password));
  if not (sfFtpResponse_isOk(Response) = sfTrue) then exit;
  WriteLn(string(sfFtpResponse_getMessage(Response)));
  WriteLn('');

  // Main menu
  repeat
    // Main FTP menu
    WriteLn('Choose an action:');
    WriteLn('1. Print working directory');
    WriteLn('2. Print contents of working directory');
    WriteLn('3. Change directory');
    WriteLn('4. Create directory');
    WriteLn('5. Delete directory');
    WriteLn('6. Rename file');
    WriteLn('7. Remove file');
    WriteLn('8. Download file');
    WriteLn('9. Upload file');
    WriteLn('0. Disconnect');
    WriteLn('');
    Write('Your choice: ');
    ReadLn(Choice);
    WriteLn('');

    case Choice of
      0:;
      1:
        begin
          // Print the current server directory
          DirectoryResponse := sfFtp_getWorkingDirectory(Server);
          WriteLn(string(sfFtpDirectoryResponse_getMessage(DirectoryResponse)));
          WriteLn('Current directory is ', string(sfFtpDirectoryResponse_getDirectory(DirectoryResponse)));
          sfFtpDirectoryResponse_destroy(DirectoryResponse);
        end;
      2:
        begin
          // Print the contents of the current server directory
          ListingResponse := sfFtp_getDirectoryListing(Server, nil);
          for Index := 0 to sfFtpListingResponse_getCount(ListingResponse) - 1 do
            WriteLn(string(sfFtpListingResponse_getName(ListingResponse, Index)));
          sfFtpListingResponse_destroy(ListingResponse);
        end;
      3:
        begin
          // Change the current directory
          Write('Choose a directory: ');
          ReadLn(Directory);
          Response := sfFtp_changeDirectory(Server, PChar(Directory));
          WriteLn(string(sfFtpResponse_getMessage(Response)));
        end;
      4:
        begin
          // Create a new directory
          Write('Name of the directory to create: ');
          ReadLn(Directory);
          Response := sfFtp_createDirectory(Server, PChar(Directory));
          WriteLn(string(sfFtpResponse_getMessage(Response)));
        end;
      5:
        begin
          // Remove an existing directory
          Write('Name of the directory to remove: ');
          ReadLn(Directory);
          Response := sfFtp_deleteDirectory(Server, PChar(Directory));
          WriteLn(string(sfFtpResponse_getMessage(Response)));
        end;
      6:
        begin
          // Rename a file
          Write('Name of the file to rename: ');
          ReadLn(Source);
          Write('New name: ');
          ReadLn(Destination);
          Response := sfFtp_renameFile(Server, PChar(Source), PChar(Destination));
          WriteLn(string(sfFtpResponse_getMessage(Response)));
        end;
      7:
        begin
          // Remove an existing directory
          Write('Name of the file to remove: ');
          ReadLn(Filename);
          Response := sfFtp_deleteFile(Server, PChar(Filename));
          WriteLn(string(sfFtpResponse_getMessage(Response)));
        end;
      8:
        begin
          // Download a file from server
          Write('Filename of the file to download (relative to current directory): ');
          ReadLn(Filename);
          Write('Directory to download the file to: ');
          ReadLn(Directory);
          Response := sfFtp_download(Server, PChar(Filename), PChar(Directory), sfFtpBinary);
          WriteLn(string(sfFtpResponse_getMessage(Response)));
        end;
      9:
        begin
          // Upload a file to server
          Write('Path of the file to upload (absolute or relative to working directory): ');
          ReadLn(Filename);
          Write('Directory to upload the file to (relative to current directory): ');
          ReadLn(Directory);
          Response := sfFtp_upload(Server, PChar(Filename), PChar(Directory), sfFtpBinary);
          WriteLn(string(sfFtpResponse_getMessage(Response)));
        end;
      else
        begin
          // Wrong choice
          WriteLn('Invalid choice!');
        end;
    end;
  until Choice = 0;

  // Disconnect from the server
  WriteLn('Disconnecting from server...');
  Response := sfFtp_disconnect(Server);
  WriteLn(string(sfFtpResponse_getMessage(Response)));

  // Wait until the user presses 'enter' key
  Write('Press enter to exit...');
  ReadLn;
end.
