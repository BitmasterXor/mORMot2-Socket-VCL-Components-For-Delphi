unit MorMotComponents;

interface

procedure Register;

implementation

uses
  Classes, MorMotClientComponent, MorMotServerComponent;

// Include the resource file that contains your component bitmaps
{$R MorMotComponents.res}

procedure Register;
begin
  // Register components with NetCom7-style names for easy migration
  RegisterComponents('mORMot2 NetCom7', [TMorMotClient, TMorMotServer]);
end;

end.
