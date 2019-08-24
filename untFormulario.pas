unit untFormulario;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Maps,
  FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  FMX.Controls.Presentation;

type
  TForm3 = class(TForm)
    lblTitle: TLabel;
    edtTitle: TEdit;
    lblDescription: TLabel;
    memDescription: TMemo;
    lblAttachments: TLabel;
    lsbAttachments: TListBox;
    btnOk: TButton;
    btnClear: TButton;
    btnSearch: TButton;
    MapView1: TMapView;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.FormCreate(Sender: TObject);
var
  Location: TMapCoordinate;
begin
  MapView1.MapType := TMapType.Normal;
  Location.Latitude := -23.548094;
  Location.Longitude := -46.635063;
  MapView1.Location := Location;

  MapView1.Zoom := 11;
end;

end.
