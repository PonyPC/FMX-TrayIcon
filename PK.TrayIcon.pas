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

unit PK.TrayIcon;

interface

uses
  System.Classes
  , FMX.Graphics
  , PK.TrayIcon.Default
  ;

type
  TTrayIcon = class
  private var
    FTrayIcon: ITrayIcon;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Apply;
    procedure AddMenu(const iName: String; const iEvent: TNotifyEvent);
    procedure EnableMenu(const iName: String; const iEnabled: Boolean);
    procedure RegisterOnClick(const iEvent: TNotifyEvent);
    procedure RegisterIcon(const iName: String; const iIcon: TBitmap);
    procedure ChangeIcon(const iName, iHint: String);
  end;

implementation

uses
  FMX.Platform
  {$IFDEF MSWINDOWS}
  , PK.TrayIcon.Win
  {$ENDIF}
  {$IFDEF OSX}
  , PK.TrayIcon.Mac
  {$ENDIF}
  ;

{ TTrayIcon }

procedure TTrayIcon.AddMenu(const iName: String; const iEvent: TNotifyEvent);
begin
  if FTrayIcon <> nil then
    FTrayIcon.AddMenu(iName, iEvent);
end;

procedure TTrayIcon.Apply;
begin
  if FTrayIcon <> nil then
    FTrayIcon.Apply;
end;

procedure TTrayIcon.ChangeIcon(const iName, iHint: String);
begin
  if FTrayIcon <> nil then
    FTrayIcon.ChangeIcon(iName, iHint);
end;

constructor TTrayIcon.Create;
var
  TrayIconFactory: ITrayIconFactory;
begin
  inherited Create;

  if
    (
      TPlatformServices.Current.SupportsPlatformService(
        ITrayIconFactory,
        IInterface(TrayIconFactory)
      )
    )
  then
    FTrayIcon := TrayIconFactory.CreateTrayIcon;
end;

destructor TTrayIcon.Destroy;
begin
  FTrayIcon := nil;

  inherited;
end;

procedure TTrayIcon.EnableMenu(const iName: String; const iEnabled: Boolean);
begin
  if FTrayIcon <> nil then
    FTrayIcon.EnableMenu(iName, iEnabled);
end;

procedure TTrayIcon.RegisterIcon(const iName: String; const iIcon: TBitmap);
begin
  if FTrayIcon <> nil then
    FTrayIcon.RegisterIcon(iName, iIcon);
end;

procedure TTrayIcon.RegisterOnClick(const iEvent: TNotifyEvent);
begin
  if FTrayIcon <> nil then
    FTrayIcon.RegisterOnClick(iEvent);
end;

end.
