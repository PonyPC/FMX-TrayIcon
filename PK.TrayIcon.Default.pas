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

unit PK.TrayIcon.Default;

interface

uses
  System.Classes
  , FMX.Graphics
  ;

type
  ITrayIcon = interface
    ['{052926DE-A337-4CC0-A9E1-0B1C3EF5FCA2}']
    procedure Apply;
    procedure AddMenu(const iName: String; const iEvent: TNotifyEvent);
    procedure EnableMenu(const iName: String; const iEnabled: Boolean);
    procedure RegisterOnClick(const iEvent: TNotifyEvent);
    procedure RegisterIcon(const iName: String; const iIcon: TBitmap);
    procedure ChangeIcon(const iName, iHint: String);
  end;

  ITrayIconFactory = interface(IInterface)
    ['{FBC54BA2-3D9B-44D0-9904-63F69B43CB2D}']
    function CreateTrayIcon: ITrayIcon;
  end;

  TTrayIconFactory = class(TInterfacedObject, ITrayIconFactory)
  public
    function CreateTrayIcon: ITrayIcon; virtual; abstract;
  end;

implementation

end.
