unit mMProtocol;

interface

uses
  System.SysUtils, System.Classes,
  mormot.core.base,
  mormot.core.log,
  mormot.net.async,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.core.text;

const
  PROTOCOL_MAGIC = $CAFEBABE;

type
  TAESMode = (
    amECB, amCBC, amCFB, amOFB, amCTR, amGCM, amCFC, amOFC, amCTC
  );

  TAESKeySize = (
    aks128, aks192, aks256
  );

  TSimpleMessage = packed record
    Magic: UInt32;
    DataSize: UInt32;
    Encrypted: Byte;  // 1 = encrypted, 0 = not encrypted
  end;

  TSendMessageCallback = procedure(const Message: RawByteString) of object;

  TEncryptionContext = record
    Enabled: Boolean;
    Mode: TAESMode;
    KeySize: TAESKeySize;
    Key: string;
    IV: string;
    procedure SetKey(const AKey: string; AMode: TAESMode; AKeySize: TAESKeySize);
    procedure SetIV(const AIV: string);
    procedure Clear;
    function GetModeString: string;
  end;

  TSimpleProtocol = class
  public
    class function PackMessage(const Data: TBytes; Encrypted: Boolean; var EncryptCtx: TEncryptionContext): RawByteString;
    class procedure SendMessage(SendCallback: TSendMessageCallback; const Data: TBytes; Encrypted: Boolean; var EncryptCtx: TEncryptionContext);
    class function TryParseMessage(const Buffer: RawByteString; out Data: TBytes; var EncryptCtx: TEncryptionContext): Boolean;
    class function HasMagicMarker(const Buffer: RawByteString): Boolean;
  end;

implementation

procedure TEncryptionContext.SetKey(const AKey: string; AMode: TAESMode; AKeySize: TAESKeySize);
begin
  Key := AKey;
  Mode := AMode;
  KeySize := AKeySize;
  Enabled := (AKey <> '');
end;

procedure TEncryptionContext.SetIV(const AIV: string);
begin
  IV := AIV;
end;

procedure TEncryptionContext.Clear;
begin
  Enabled := False;
  Key := '';
  IV := '';
end;

function TEncryptionContext.GetModeString: string;
const
  ModeNames: array[TAESMode] of string = (
    'ECB', 'CBC', 'CFB', 'OFB', 'CTR', 'GCM', 'CFC', 'OFC', 'CTC'
  );
  KeySizeNames: array[TAESKeySize] of string = ('128', '192', '256');
begin
  if Enabled then
    Result := Format('AES-%s-%s', [KeySizeNames[KeySize], ModeNames[Mode]])
  else
    Result := 'DISABLED';
end;

class function TSimpleProtocol.PackMessage(const Data: TBytes; Encrypted: Boolean; var EncryptCtx: TEncryptionContext): RawByteString;
var
  Header: TSimpleMessage;
  DataStr: RawByteString;
  Password: RawByteString;
  KeyBytes: THash256;
  AES: TAesAbstract;
  KeySizeBits: Integer;
  Salt: RawByteString;
  WillEncrypt: Boolean;
begin
  Header.Magic := PROTOCOL_MAGIC;

  // Convert TBytes to RawByteString
  SetLength(DataStr, Length(Data));
  if Length(Data) > 0 then
    Move(Data[0], DataStr[1], Length(Data));

  // Determine if we will actually encrypt this message
  WillEncrypt := Encrypted and EncryptCtx.Enabled and (EncryptCtx.Key <> '');

  // Set the encryption flag for this specific message
  Header.Encrypted := Ord(WillEncrypt);

  // Encrypt ONLY if requested AND encryption is enabled
  if WillEncrypt then
  begin
    // Determine key size bits
    case EncryptCtx.KeySize of
      aks128: KeySizeBits := 128;
      aks192: KeySizeBits := 192;
      aks256: KeySizeBits := 256;
    else
      KeySizeBits := 256;
    end;

    // Convert password to RawByteString and derive key
    Password := ToUtf8(EncryptCtx.Key);
    Salt := 'mormot_protocol_salt_2024';
    Pbkdf2HmacSha256(Password, Salt, 1000, KeyBytes);

    try
      // Create AES instance based on mode - exactly like your demo
      case EncryptCtx.Mode of
        amECB: AES := TAesEcb.Create(KeyBytes, KeySizeBits);
        amCBC: AES := TAesCbc.Create(KeyBytes, KeySizeBits);
        amCFB: AES := TAesCfb.Create(KeyBytes, KeySizeBits);
        amOFB: AES := TAesOfb.Create(KeyBytes, KeySizeBits);
        amCTR: AES := TAesCtr.Create(KeyBytes, KeySizeBits);
        amGCM: AES := TAesGcm.Create(KeyBytes, KeySizeBits);
        amCFC: AES := TAesCfc.Create(KeyBytes, KeySizeBits);
        amOFC: AES := TAesOfc.Create(KeyBytes, KeySizeBits);
        amCTC: AES := TAesCtc.Create(KeyBytes, KeySizeBits);
      else
        AES := TAesEcb.Create(KeyBytes, KeySizeBits);
      end;

      try
        // Encrypt using the EXACT same pattern as your demo
        DataStr := AES.EncryptPkcs7(DataStr, True); // Always use random IV
      except
        on E: Exception do
        begin
          TSynLog.Add.Log(sllError, 'Encryption failed: %', [E.Message]);
        end;
      end;

      AES.Free;
    except
      on E: Exception do
      begin
        TSynLog.Add.Log(sllError, 'AES creation failed: %', [E.Message]);
      end;
    end;

    // Clear sensitive key material
    FillChar(KeyBytes, SizeOf(KeyBytes), 0);
  end;

  Header.DataSize := Length(DataStr);

  // Pack header + data
  SetLength(Result, SizeOf(Header) + Length(DataStr));
  Move(Header, Result[1], SizeOf(Header));
  if Length(DataStr) > 0 then
    Move(DataStr[1], Result[SizeOf(Header) + 1], Length(DataStr));
end;

class procedure TSimpleProtocol.SendMessage(SendCallback: TSendMessageCallback; const Data: TBytes; Encrypted: Boolean; var EncryptCtx: TEncryptionContext);
begin
  if Assigned(SendCallback) then
    SendCallback(PackMessage(Data, Encrypted, EncryptCtx));
end;

class function TSimpleProtocol.TryParseMessage(const Buffer: RawByteString; out Data: TBytes; var EncryptCtx: TEncryptionContext): Boolean;
var
  Header: TSimpleMessage;
  DataStr: RawByteString;
  Password: RawByteString;
  KeyBytes: THash256;
  AES: TAesAbstract;
  KeySizeBits: Integer;
  Salt: RawByteString;
begin
  Result := False;
  SetLength(Data, 0);

  if Length(Buffer) < SizeOf(Header) then Exit;

  Move(Buffer[1], Header, SizeOf(Header));
  if Header.Magic <> PROTOCOL_MAGIC then Exit;
  if Length(Buffer) < SizeOf(Header) + Header.DataSize then Exit;

  // Extract data
  SetLength(DataStr, Header.DataSize);
  if Header.DataSize > 0 then
    Move(Buffer[SizeOf(Header) + 1], DataStr[1], Header.DataSize);

  // FIXED: Only decrypt if THIS SPECIFIC MESSAGE was encrypted
  // Check the message's encryption flag, not just the context
  if (Header.Encrypted = 1) and EncryptCtx.Enabled and (EncryptCtx.Key <> '') then
  begin
    // Determine key size bits
    case EncryptCtx.KeySize of
      aks128: KeySizeBits := 128;
      aks192: KeySizeBits := 192;
      aks256: KeySizeBits := 256;
    else
      KeySizeBits := 256;
    end;

    // Convert password to RawByteString and derive key
    Password := ToUtf8(EncryptCtx.Key);
    Salt := 'mormot_protocol_salt_2024';
    Pbkdf2HmacSha256(Password, Salt, 1000, KeyBytes);

    try
      // Create AES instance based on mode - exactly like your demo
      case EncryptCtx.Mode of
        amECB: AES := TAesEcb.Create(KeyBytes, KeySizeBits);
        amCBC: AES := TAesCbc.Create(KeyBytes, KeySizeBits);
        amCFB: AES := TAesCfb.Create(KeyBytes, KeySizeBits);
        amOFB: AES := TAesOfb.Create(KeyBytes, KeySizeBits);
        amCTR: AES := TAesCtr.Create(KeyBytes, KeySizeBits);
        amGCM: AES := TAesGcm.Create(KeyBytes, KeySizeBits);
        amCFC: AES := TAesCfc.Create(KeyBytes, KeySizeBits);
        amOFC: AES := TAesOfc.Create(KeyBytes, KeySizeBits);
        amCTC: AES := TAesCtc.Create(KeyBytes, KeySizeBits);
      else
        AES := TAesEcb.Create(KeyBytes, KeySizeBits);
      end;

      try
        // Decrypt using the EXACT same pattern as your demo
        DataStr := AES.DecryptPkcs7(DataStr, True); // Random IV was used
      except
        on E: Exception do
        begin
          DataStr := '[DECRYPT_FAILED] ' + E.Message;
          TSynLog.Add.Log(sllError, 'Decryption failed: %', [E.Message]);
        end;
      end;

      AES.Free;
    except
      on E: Exception do
      begin
        DataStr := '[AES_CREATION_FAILED] ' + E.Message;
        TSynLog.Add.Log(sllError, 'AES creation failed: %', [E.Message]);
      end;
    end;

    // Clear sensitive key material
    FillChar(KeyBytes, SizeOf(KeyBytes), 0);
  end;
  // If Header.Encrypted = 0, data stays as plain text - no decryption needed!

  // Convert back to TBytes
  SetLength(Data, Length(DataStr));
  if Length(DataStr) > 0 then
    Move(DataStr[1], Data[0], Length(DataStr));

  Result := True;
end;

class function TSimpleProtocol.HasMagicMarker(const Buffer: RawByteString): Boolean;
begin
  Result := (Length(Buffer) >= 4) and (PUInt32(@Buffer[1])^ = PROTOCOL_MAGIC);
end;

end.
