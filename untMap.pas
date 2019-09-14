unit untMap;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Maps,
  System.Sensors, System.Sensors.Components, System.Permissions,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts;

type
  TFormMapa = class(TForm)
    MapView1: TMapView;
    LocationSensor1: TLocationSensor;
    Timer1: TTimer;
    Button1: TButton;
    Button2: TButton;
    ActionSheet: TLayout;
    GrayBackground: TRectangle;
    Rectangle1: TRectangle;
    procedure MapView1MarkerClick(Marker: TMapMarker);
    procedure LocationSensor1LocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure GrayBackgroundClick(Sender: TObject);
  private
    { Private declarations }

    {$IFDEF ANDROID}
     Access_Fine_Location, Access_Coarse_Location : string;
     procedure DisplayRationale(Sender: TObject;
              const APermissions: TArray<string>; const APostRationaleProc: TProc);
     procedure LocationPermissionRequestResult
                (Sender: TObject; const APermissions: TArray<string>;
                const AGrantResults: TArray<TPermissionStatus>);
    {$ENDIF}
  public
    { Public declarations }
  end;

var
  FormMapa: TFormMapa;

implementation

uses FMX.DialogService
{$IFDEF ANDROID}
, Androidapi.Helpers
, Androidapi.JNI.JavaTypes
, Androidapi.JNI.Os
{$ENDIF}
{$IFDEF MSWINDOWS}
, System.Win.Sensors
{$ENDIF}

;

{$R *.fmx}
{$IFDEF ANDROID}

procedure TFormMapa.Button1Click(Sender: TObject);
begin
  ShowMessage(MapView1.Zoom.ToString);
end;

procedure TFormMapa.Button2Click(Sender: TObject);
begin
  ActionSheet.Visible := True;
  Exit;
end;

procedure TFormMapa.DisplayRationale(Sender: TObject;
  const APermissions: TArray<string>; const APostRationaleProc: TProc);
var
  I: Integer;
  RationaleMsg: string;
begin
  for I := 0 to High(APermissions) do
  begin
    if (APermissions[I] = Access_Coarse_Location) or (APermissions[I] = Access_Fine_Location) then
      RationaleMsg := 'O app precisa de acesso ao GPS para obter sua localização'
  end;

  TDialogService.ShowMessage(RationaleMsg,
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;

procedure TFormMapa.LocationPermissionRequestResult
  (Sender: TObject; const APermissions: TArray<string>;
const AGrantResults: TArray<TPermissionStatus>);
var
         x : integer;
begin
  if (Length(AGrantResults) = 2) and
    (AGrantResults[0] = TPermissionStatus.Granted) and
    (AGrantResults[1] = TPermissionStatus.Granted) then
    LocationSensor1.Active := true
  else
  begin
//    Switch.IsChecked := false;
    TDialogService.ShowMessage
      ('Não é possível acessar o GPS porque o app não possui acesso')
  end;

end;

{$ENDIF}

procedure TFormMapa.Button2Click(Sender: TObject);
begin
  ActionSheet.Visible := True;
end;

procedure TFormMapa.FormCreate(Sender: TObject);
begin
  {$IFDEF ANDROID}
  Access_Coarse_Location := JStringToString(TJManifest_permission.JavaClass.ACCESS_COARSE_LOCATION);
  Access_Fine_Location := JStringToString(TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION);
  {$ENDIF}
end;

procedure TFormMapa.GrayBackgroundClick(Sender: TObject);
begin
  ActionSheet.Visible := False;
end;

procedure TFormMapa.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
var
  Location: TMapCoordinate;
begin
  Location.Latitude := NewLocation.Latitude;
  Location.Longitude := NewLocation.Longitude;
  MapView1.Location := Location;
end;

procedure TFormMapa.MapView1MarkerClick(Marker: TMapMarker);
begin
  ShowMessage(marker.Descriptor.Title);
end;

procedure TFormMapa.Timer1Timer(Sender: TObject);
begin
  {$IFDEF ANDROID}
  PermissionsService.RequestPermissions([Access_Coarse_Location,
                                         Access_Fine_Location],
                                         LocationPermissionRequestResult,
                                         DisplayRationale);
  {$ENDIF}

  {$IFDEF IOS}
  LocationSensor1.Active := true;
  {$ENDIF}

  Timer1.Enabled := False;
end;

end.
