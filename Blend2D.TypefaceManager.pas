unit Blend2D.TypefaceManager;

interface

uses
  System.SysUtils,
  Blend2D;

type
  TBLTypefaceManager = class abstract
  {$REGION 'Internal Declarations'}
  private class var
    FInstance: TBLTypefaceManager;
  {$ENDREGION 'Internal Declarations'}
  public
    function GetTypefaceData(const AFamilyName: String;
      const AProps: TBLFontQueryProperties;
      out AFaceIndex: Integer): TBytes; virtual; abstract;

    class property Instance: TBLTypefaceManager read FInstance;
  end;

implementation

uses
  {$IF Defined(MSWINDOWS)}
  Blend2D.TypefaceManager.Windows;
  {$ELSE}
    {$MESSAGE Error 'Unsupported platform'}
  {$ENDIF}

initialization
  {$IF Defined(MSWINDOWS)}
  TBLTypefaceManager.FInstance := TBLTypefaceManagerWindows.Create;
  {$ELSE}
    {$MESSAGE Error 'Unsupported platform'}
  {$ENDIF}

finalization
  TBLTypefaceManager.FInstance.Free;
  TBLTypefaceManager.FInstance := nil;

end.
