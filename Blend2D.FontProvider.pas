unit Blend2D.FontProvider;

interface

uses
  System.SysUtils,
  Blend2D;

type
  TBLFontProvider = class abstract
  {$REGION 'Internal Declarations'}
  private class var
    FInstance: TBLFontProvider;
  {$ENDREGION 'Internal Declarations'}
  public
    function GetTypefaceData(const AFamilyName: String;
      const AProps: TBLFontQueryProperties;
      out AFaceIndex: Integer): TBytes; virtual; abstract;

    class property Instance: TBLFontProvider read FInstance;
  end;

implementation

uses
  {$IF Defined(MSWINDOWS)}
  Blend2D.FontProvider.Windows;
  {$ELSE}
    {$MESSAGE Error 'Unsupported platform'}
  {$ENDIF}

initialization
  {$IF Defined(MSWINDOWS)}
  TBLFontProvider.FInstance := TBLFontProviderWindows.Create;
  {$ELSE}
    {$MESSAGE Error 'Unsupported platform'}
  {$ENDIF}

finalization
  TBLFontProvider.FInstance.Free;
  TBLFontProvider.FInstance := nil;

end.
