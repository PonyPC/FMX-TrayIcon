(*
  * TrayIcon / StatusBar Icon Utility
  *
  * PLATFORMS
  *   Windows / macOS
  *
  * LICENSE
  *   Copyright (c) 2018 HOSOKAWA Jun
  *   Released under the MIT license
  *   http://opensource.org/licenses/mit-license.php
  *
  * HOW TO USE
  *   uses PK.TrayIcon;
  *
  *   type
  *     TForm1 = class(TForm)
  *       procedure FormCreate(Sender: TObject);
  *     private
  *       FTray: TTrayIcon;
  *     end;
  *
  *   procedure TForm1.FormCreate(Sender: TObject);
  *   begin
  *     FTray := TTrayIcon.Create;
  *     FTray.AddMenu('Foo', FooClick);    // Right Click Menu
  *     FTray.RegisterIcon('Bar', BarBmp); // BarBmp is TBitmap Instance
  *     FTray.RegisterOnClick(TrayClick);  // TrayIcon Clicked Event (Win Only)
  *     FTray.Apply;
  *   end;
  *
  * 2018/04/17 Version 1.0.0
  * Programmed by HOSOKAWA Jun (twitter: @pik)
*)

unit PK.TrayIcon.Win;

{$IFNDEF MSWINDOWS}
{$WARNINGS OFF 1011}

interface

implementation

end.
{$ENDIF}
  interface

implementation

uses
  Winapi.Windows, System.Classes, System.SysUtils, System.UITypes, System.Generics.Collections, FMX.Platform,
  FMX.Platform.Win,
  FMX.Graphics, FMX.Surfaces, FMX.Forms, Vcl.ExtCtrls, Vcl.Menus, Vcl.Graphics, PK.TrayIcon.Default;

type
  TTrayIconWin = class(TInterfacedObject, ITrayIcon)
  private
  var
    FTrayIcon: TTrayIcon;
    FMenu: TPopupMenu;
    FIcons: TDictionary<String, TIcon>;
  private
    function FindMenu(const iName: String): TMenuItem;
    function CreateIcon(const iIcon: FMX.Graphics.TBitmap): TIcon;
    procedure ClearIcon;
    procedure CreateMaskBitmap(const iMask, iSource: TBitmap);
    procedure TrayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Apply(const iTitle: String);
    procedure AddMenu(const iName: String; const iEvent: TNotifyEvent);
    procedure EnableMenu(const iName: String; const iEnabled: Boolean);
    procedure RegisterOnClick(const iEvent: TNotifyEvent);
    procedure RegisterOnDblClick(const iEvent: TNotifyEvent);
    procedure RegisterIcon(const iName: String; const iIcon: FMX.Graphics.TBitmap);
    procedure ChangeIcon(const iName: String; const iconForm: TForm);
    procedure BalloonHint(const iTitle, iContent: String; const iconType: Integer; const mTimeout: Integer);
  end;

  TTrayIconFactoryWin = class(TTrayIconFactory)
  public
    function CreateTrayIcon: ITrayIcon; override;
  end;

procedure RegisterTrayIconWin;
begin
  TPlatformServices.Current.AddPlatformService(ITrayIconFactory, TTrayIconFactoryWin.Create);
end;

{ TTrayIconWin }

procedure TTrayIconWin.AddMenu(const iName: String; const iEvent: TNotifyEvent);
var
  Item: TMenuItem;
begin
  if iName = '-' then
    Item := NewLine
  else
    Item := NewItem(iName, 0, False, True, iEvent, 0, Format('Item%d', [FMenu.Items.Count]));

  FMenu.Items.Add(Item);
end;

procedure TTrayIconWin.Apply(const iTitle: String);
begin
  FTrayIcon.Hint := iTitle;
  FTrayIcon.SetDefaultIcon;
  FTrayIcon.Visible := True;
end;

procedure TTrayIconWin.BalloonHint(const iTitle, iContent: String; const iconType: Integer; const mTimeout: Integer);
begin
  FTrayIcon.BalloonTitle := iTitle;
  FTrayIcon.BalloonHint := iContent;
  FTrayIcon.BalloonFlags := TBalloonFlags(iconType);
  FTrayIcon.BalloonTimeout := mTimeout;
  FTrayIcon.Animate := True;
  FTrayIcon.AnimateInterval := 1000;
  FTrayIcon.ShowBalloonHint;
end;

procedure TTrayIconWin.ChangeIcon(const iName: String; const iconForm: TForm);
var
  Icon: TIcon;
begin
  if iconForm <> nil then
  begin
    FTrayIcon.Icon.Handle := GetClassLong(FormToHWnd(iconForm), GCL_HICONSM);
    Exit;
  end;
  if FIcons.TryGetValue(iName, Icon) then
    FTrayIcon.Icon := Icon;

  // FTrayIcon.Hint := iHint;
end;

procedure TTrayIconWin.ClearIcon;
var
  Pair: TPair<String, TIcon>;
begin
  for Pair in FIcons do
    Pair.Value.DisposeOf;

  FIcons.Clear;
end;

constructor TTrayIconWin.Create;
begin
  inherited Create;

  FIcons := TDictionary<String, TIcon>.Create;

  FMenu := TPopupMenu.Create(nil);

  FTrayIcon := TTrayIcon.Create(nil);
  FTrayIcon.OnMouseDown := TrayMouseDown;
end;

function TTrayIconWin.CreateIcon(const iIcon: FMX.Graphics.TBitmap): TIcon;
var
  Bmp, Mask: TBitmap;
  MS: TMemoryStream;
  Surface: TBitmapSurface;
  IconInfo: TIconInfo;
begin
  Result := TIcon.Create;

  Bmp := TBitmap.Create;
  try
    MS := TMemoryStream.Create;
    try
      Surface := TBitmapSurface.Create;
      try
        Surface.Assign(iIcon);
        TBitmapCodecManager.SaveToStream(MS, Surface, '.bmp');
      finally
        Surface.DisposeOf;
      end;

      MS.Position := 0;
      Bmp.LoadFromStream(MS);
    finally
      MS.DisposeOf;
    end;

    Mask := TBitmap.Create;
    try
      CreateMaskBitmap(Mask, Bmp);

      IconInfo.fIcon := True;
      IconInfo.xHotspot := 0;
      IconInfo.yHotspot := 0;
      IconInfo.hbmMask := Mask.Handle;
      IconInfo.hbmColor := Bmp.Handle;

      Result.Handle := CreateIconIndirect(IconInfo);
    finally
      Mask.DisposeOf;
    end;
  finally
    Bmp.DisposeOf;
  end;
end;

procedure TTrayIconWin.CreateMaskBitmap(const iMask, iSource: TBitmap);
type
  TRGBA = record
    B: Byte;
    G: Byte;
    R: Byte;
    A: Byte;
  end;

  TRGBAArray = array [0 .. 0] of TRGBA;
  PRGBAArray = ^TRGBAArray;
var
  W, H: Integer;
  X, Y: Integer;
  SourceScan, MaskScan: PRGBAArray;
begin
  W := iSource.Width;
  H := iSource.Height;

  iMask.SetSize(W, H);

  iMask.Canvas.Lock;
  try
    for Y := 0 to H - 1 do
    begin
      MaskScan := iMask.ScanLine[Y];
      SourceScan := iSource.ScanLine[Y];

      for X := 0 to W - 1 do
        if SourceScan[X].A = 0 then
          DWORD(MaskScan[X]) := 0
        else
          DWORD(MaskScan[X]) := $FFFFFFFF;
    end;
  finally
    iMask.Canvas.Unlock;
  end;
end;

destructor TTrayIconWin.Destroy;
begin
  ClearIcon;

  FTrayIcon.DisposeOf;
  FMenu.DisposeOf;
  FIcons.DisposeOf;

  inherited;
end;

procedure TTrayIconWin.EnableMenu(const iName: String; const iEnabled: Boolean);
var
  Item: TMenuItem;
begin
  Item := FindMenu(iName);
  if Item <> nil then
    Item.Enabled := iEnabled;
end;

function TTrayIconWin.FindMenu(const iName: String): TMenuItem;

  procedure FindNamedItem(const iRoot: TMenuItem);
  var
    Item: TMenuItem;
    i: Integer;
  begin
    for i := 0 to iRoot.Count - 1 do
    begin
      Item := iRoot.Items[i];
      if Item.Caption = iName then
      begin
        Result := Item;
        Break;
      end
      else
        FindNamedItem(Item);
    end;
  end;

begin
  Result := nil;
  FindNamedItem(FMenu.Items);
end;

procedure TTrayIconWin.RegisterIcon(const iName: String; const iIcon: FMX.Graphics.TBitmap);
begin
  FIcons.Add(iName, CreateIcon(iIcon));
end;

procedure TTrayIconWin.RegisterOnClick(const iEvent: TNotifyEvent);
begin
  FTrayIcon.OnClick := iEvent;
end;

procedure TTrayIconWin.RegisterOnDblClick(const iEvent: TNotifyEvent);
begin
  FTrayIcon.OnDblClick := iEvent;
end;

procedure TTrayIconWin.TrayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton.mbRight then
    FMenu.Popup(X, Y);
end;

{ TTrayIconFactoryWin }

function TTrayIconFactoryWin.CreateTrayIcon: ITrayIcon;
begin
  Result := TTrayIconWin.Create;
end;

initialization

RegisterTrayIconWin;

end.
