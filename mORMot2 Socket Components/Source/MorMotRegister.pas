unit MorMotRegister;

interface

procedure Register;

implementation

uses
  Classes, mMClient, mMServer;

// Include the resource file that contains component bitmaps
{$R MorMotComponents.res}

procedure Register;
begin
  // Register components
  RegisterComponents('mORMot2 Components', [TmMClient, TmMServer]);
end;

end.
