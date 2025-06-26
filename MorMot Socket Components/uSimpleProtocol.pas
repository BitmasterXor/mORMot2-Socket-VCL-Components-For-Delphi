unit uSimpleProtocol;

interface

uses
  System.SysUtils, System.Classes,
  mormot.core.base,
  mormot.core.log,
  mormot.net.async;

const
  // Magic marker for our protocol messages
  PROTOCOL_MAGIC = $CAFEBABE;

  // NetCom7-style command types
  NETCOM_CMD_COMMAND = $01;
  NETCOM_CMD_RESULT = $02;
  NETCOM_CMD_EXCEPTION = $03;

type
  // Simple message structure - HEADER + DATA
  TSimpleMessage = packed record
    Magic: UInt32;        // Magic marker
    DataSize: UInt32;     // Size of data that follows
    // Binary data follows after this header
  end;

  // NetCom7-style command structure
  TNetComCommand = packed record
    CommandType: Byte;    // NETCOM_CMD_* constants
    Cmd: Integer;         // Command number
    RequiresResult: Boolean; // Whether result is expected
    DataSize: UInt32;     // Size of command data
    // Command data follows
  end;

  // Send callback function type
  TSendMessageCallback = procedure(const Message: RawByteString) of object;

  // NetCom7-style protocol - BINARY ONLY!
  TSimpleProtocol = class
  public
    // Original send methods for backward compatibility
    class procedure Send(SendCallback: TSendMessageCallback; const Data: RawByteString); overload;
    class procedure Send(SendCallback: TSendMessageCallback; const Data: TBytes); overload;
    class procedure Send(SendCallback: TSendMessageCallback; const Data: string); overload;
    class procedure Send(SendCallback: TSendMessageCallback; const Data; Size: Integer); overload;

    // NetCom7-style command packing/unpacking
    class function PackCommand(aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean): RawByteString;
    class function PackResult(const aData: TBytes): RawByteString;
    class function PackException(const aMessage: string): RawByteString;

    class function TryUnpackCommand(const Buffer: RawByteString; out aCmd: Integer; out aData: TBytes; out aRequiresResult: Boolean): Boolean;
    class function TryUnpackResult(const Buffer: RawByteString; out aData: TBytes): Boolean;
    class function TryUnpackException(const Buffer: RawByteString; out aMessage: string): Boolean;

    // Original message parsing
    class function TryParseMessage(const Buffer: RawByteString; out Data: RawByteString): Boolean;
    class function HasMagicMarker(const Buffer: RawByteString): Boolean;

    // Convert received binary to whatever you want
    class function AsString(const Data: RawByteString): string;
    class function AsBytes(const Data: RawByteString): TBytes;
    class function AsInteger(const Data: RawByteString): Integer;
    class function AsFloat(const Data: RawByteString): Double;

    // Utility conversions
    class function BytesToRawByteString(const Data: TBytes): RawByteString;
    class function RawByteStringToBytes(const Data: RawByteString): TBytes;
  end;

implementation

{ TSimpleProtocol }

class procedure TSimpleProtocol.Send(SendCallback: TSendMessageCallback; const Data: RawByteString);
var
  Header: TSimpleMessage;
  Message: RawByteString;
begin
  Header.Magic := PROTOCOL_MAGIC;
  Header.DataSize := Length(Data);

  SetLength(Message, SizeOf(Header) + Length(Data));
  Move(Header, Message[1], SizeOf(Header));
  if Length(Data) > 0 then
    Move(Data[1], Message[SizeOf(Header) + 1], Length(Data));

  if Assigned(SendCallback) then
    SendCallback(Message);
end;

class procedure TSimpleProtocol.Send(SendCallback: TSendMessageCallback; const Data: TBytes);
begin
  Send(SendCallback, BytesToRawByteString(Data));
end;

class procedure TSimpleProtocol.Send(SendCallback: TSendMessageCallback; const Data: string);
begin
  Send(SendCallback, RawByteString(UTF8Encode(Data)));
end;

class procedure TSimpleProtocol.Send(SendCallback: TSendMessageCallback; const Data; Size: Integer);
var
  Bytes: TBytes;
begin
  SetLength(Bytes, Size);
  Move(Data, Bytes[0], Size);
  Send(SendCallback, Bytes);
end;

// NetCom7-style command packing
class function TSimpleProtocol.PackCommand(aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean): RawByteString;
var
  Command: TNetComCommand;
  CommandData: RawByteString;
  Header: TSimpleMessage;
begin
  Command.CommandType := NETCOM_CMD_COMMAND;
  Command.Cmd := aCmd;
  Command.RequiresResult := aRequiresResult;
  Command.DataSize := Length(aData);

  SetLength(CommandData, SizeOf(Command) + Length(aData));
  Move(Command, CommandData[1], SizeOf(Command));
  if Length(aData) > 0 then
    Move(aData[0], CommandData[SizeOf(Command) + 1], Length(aData));

  // Wrap in standard message format manually
  Header.Magic := PROTOCOL_MAGIC;
  Header.DataSize := Length(CommandData);

  SetLength(Result, SizeOf(Header) + Length(CommandData));
  Move(Header, Result[1], SizeOf(Header));
  if Length(CommandData) > 0 then
    Move(CommandData[1], Result[SizeOf(Header) + 1], Length(CommandData));
end;

class function TSimpleProtocol.PackResult(const aData: TBytes): RawByteString;
var
  Command: TNetComCommand;
  CommandData: RawByteString;
  Header: TSimpleMessage;
begin
  Command.CommandType := NETCOM_CMD_RESULT;
  Command.Cmd := 0;
  Command.RequiresResult := False;
  Command.DataSize := Length(aData);

  SetLength(CommandData, SizeOf(Command) + Length(aData));
  Move(Command, CommandData[1], SizeOf(Command));
  if Length(aData) > 0 then
    Move(aData[0], CommandData[SizeOf(Command) + 1], Length(aData));

  // Wrap in standard message format manually
  Header.Magic := PROTOCOL_MAGIC;
  Header.DataSize := Length(CommandData);

  SetLength(Result, SizeOf(Header) + Length(CommandData));
  Move(Header, Result[1], SizeOf(Header));
  if Length(CommandData) > 0 then
    Move(CommandData[1], Result[SizeOf(Header) + 1], Length(CommandData));
end;

class function TSimpleProtocol.PackException(const aMessage: string): RawByteString;
var
  Command: TNetComCommand;
  CommandData: RawByteString;
  MessageBytes: TBytes;
  Header: TSimpleMessage;
begin
  MessageBytes := TEncoding.UTF8.GetBytes(aMessage);

  Command.CommandType := NETCOM_CMD_EXCEPTION;
  Command.Cmd := 0;
  Command.RequiresResult := False;
  Command.DataSize := Length(MessageBytes);

  SetLength(CommandData, SizeOf(Command) + Length(MessageBytes));
  Move(Command, CommandData[1], SizeOf(Command));
  if Length(MessageBytes) > 0 then
    Move(MessageBytes[0], CommandData[SizeOf(Command) + 1], Length(MessageBytes));

  // Wrap in standard message format manually
  Header.Magic := PROTOCOL_MAGIC;
  Header.DataSize := Length(CommandData);

  SetLength(Result, SizeOf(Header) + Length(CommandData));
  Move(Header, Result[1], SizeOf(Header));
  if Length(CommandData) > 0 then
    Move(CommandData[1], Result[SizeOf(Header) + 1], Length(CommandData));
end;

class function TSimpleProtocol.TryUnpackCommand(const Buffer: RawByteString; out aCmd: Integer; out aData: TBytes; out aRequiresResult: Boolean): Boolean;
var
  Command: TNetComCommand;
begin
  Result := False;
  aCmd := 0;
  SetLength(aData, 0);
  aRequiresResult := False;

  if Length(Buffer) < SizeOf(Command) then Exit;

  Move(Buffer[1], Command, SizeOf(Command));
  if Command.CommandType <> NETCOM_CMD_COMMAND then Exit;
  if Length(Buffer) < SizeOf(Command) + Command.DataSize then Exit;

  aCmd := Command.Cmd;
  aRequiresResult := Command.RequiresResult;

  if Command.DataSize > 0 then
  begin
    SetLength(aData, Command.DataSize);
    Move(Buffer[SizeOf(Command) + 1], aData[0], Command.DataSize);
  end;

  Result := True;
end;

class function TSimpleProtocol.TryUnpackResult(const Buffer: RawByteString; out aData: TBytes): Boolean;
var
  Command: TNetComCommand;
begin
  Result := False;
  SetLength(aData, 0);

  if Length(Buffer) < SizeOf(Command) then Exit;

  Move(Buffer[1], Command, SizeOf(Command));
  if Command.CommandType <> NETCOM_CMD_RESULT then Exit;
  if Length(Buffer) < SizeOf(Command) + Command.DataSize then Exit;

  if Command.DataSize > 0 then
  begin
    SetLength(aData, Command.DataSize);
    Move(Buffer[SizeOf(Command) + 1], aData[0], Command.DataSize);
  end;

  Result := True;
end;

class function TSimpleProtocol.TryUnpackException(const Buffer: RawByteString; out aMessage: string): Boolean;
var
  Command: TNetComCommand;
  MessageBytes: TBytes;
begin
  Result := False;
  aMessage := '';

  if Length(Buffer) < SizeOf(Command) then Exit;

  Move(Buffer[1], Command, SizeOf(Command));
  if Command.CommandType <> NETCOM_CMD_EXCEPTION then Exit;
  if Length(Buffer) < SizeOf(Command) + Command.DataSize then Exit;

  if Command.DataSize > 0 then
  begin
    SetLength(MessageBytes, Command.DataSize);
    Move(Buffer[SizeOf(Command) + 1], MessageBytes[0], Command.DataSize);
    aMessage := TEncoding.UTF8.GetString(MessageBytes);
  end;

  Result := True;
end;

class function TSimpleProtocol.TryParseMessage(const Buffer: RawByteString; out Data: RawByteString): Boolean;
var
  Header: TSimpleMessage;
begin
  Result := False;
  Data := '';

  if Length(Buffer) < SizeOf(Header) then Exit;
  Move(Buffer[1], Header, SizeOf(Header));
  if Header.Magic <> PROTOCOL_MAGIC then Exit;
  if Length(Buffer) < SizeOf(Header) + Header.DataSize then Exit;

  if Header.DataSize > 0 then
  begin
    SetLength(Data, Header.DataSize);
    Move(Buffer[SizeOf(Header) + 1], Data[1], Header.DataSize);
  end;

  Result := True;
end;

class function TSimpleProtocol.HasMagicMarker(const Buffer: RawByteString): Boolean;
begin
  Result := (Length(Buffer) >= 4) and (PUInt32(@Buffer[1])^ = PROTOCOL_MAGIC);
end;

// Convert received binary to whatever you want
class function TSimpleProtocol.AsString(const Data: RawByteString): string;
begin
  Result := UTF8ToString(Data);
end;

class function TSimpleProtocol.AsBytes(const Data: RawByteString): TBytes;
begin
  Result := RawByteStringToBytes(Data);
end;

class function TSimpleProtocol.AsInteger(const Data: RawByteString): Integer;
begin
  if Length(Data) >= SizeOf(Integer) then
    Result := PInteger(@Data[1])^
  else
    Result := 0;
end;

class function TSimpleProtocol.AsFloat(const Data: RawByteString): Double;
begin
  if Length(Data) >= SizeOf(Double) then
    Result := PDouble(@Data[1])^
  else
    Result := 0.0;
end;

class function TSimpleProtocol.BytesToRawByteString(const Data: TBytes): RawByteString;
begin
  SetLength(Result, Length(Data));
  if Length(Data) > 0 then
    Move(Data[0], Result[1], Length(Data));
end;

class function TSimpleProtocol.RawByteStringToBytes(const Data: RawByteString): TBytes;
begin
  SetLength(Result, Length(Data));
  if Length(Data) > 0 then
    Move(Data[1], Result[0], Length(Data));
end;

end.
