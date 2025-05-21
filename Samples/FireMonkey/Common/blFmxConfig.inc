{ Undefine to disable Skia rendering for performance comparison }
{$DEFINE USE_SKIA}

{$IF Defined(IOS) or Defined(ANDROID)}
  {$DEFINE MOBILE}
{$ENDIF}