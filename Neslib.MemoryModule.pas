unit Neslib.MemoryModule;
{ Partical Delphi port of https://github.com/fancycode/MemoryModule }

{$IFNDEF MSWINDOWS}
  {$MESSAGE Error 'This unit only works on Windows.'}
{$ENDIF}

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Generics.Defaults;

type
  TMemoryModule = class
  {$REGION 'Internal Declarations'}
  private type
    TSectionFinalizeData = record
    public
      Address: Pointer;
      AlignedAddress: Pointer;
      Size: UIntPtr;
      Characteristics: Cardinal;
      Last: Boolean;
    end;
  private type
    TExportNameEntry = record
    public
      Name: LPCSTR;
      Idx: Word;
    end;
    PExportNameEntry = ^TExportNameEntry;
  private type
    TDllEntryProc = function(AInstDll: THandle; AReason: DWORD; AReserved: Pointer): BOOL; stdcall;
  {$IFDEF WIN64}
  private type
    PPointerList = ^TPointerList;
    TPointerList = record
    public
      Next: PPointerList;
      Address: Pointer;
    end;
  {$ENDIF}
  private class var
    FComparer: IComparer<TExportNameEntry>;
  private
    FModules: TArray<HMODULE>;
    FNameExportsTable: TArray<TExportNameEntry>;
    FCodeBase: Pointer;
    FHeaders: PImageNtHeaders;
    FPageSize: Cardinal;
    {$IFDEF WIN64}
    FBlockedMemory: PPointerList;
    {$ENDIF}
    FIsRelocated: Boolean;
    FInitialized: Boolean;
    FFreeMemoryOnly: Boolean;
  private
    class procedure CheckSize(const ASize, AExpected: IntPtr); static;
    class function ImageFirstSection(const AHeader: PImageNtHeaders): PImageSectionHeader; inline; static;
    class function AlignValueUp(const AValue, AAlignment: UIntPtr): UIntPtr; inline; static;
    class function AlignValueDown(const AValue, AAlignment: UIntPtr): UIntPtr; inline; static;
    class function AlignAddressDown(const AAddress: Pointer;
      const AAlignment: UIntPtr): Pointer; inline; static;
    class function OffsetPointer(const AData: Pointer; const AOffset: IntPtr): Pointer; inline; static;
  private
    procedure CopySections(const AData: PByte; const ASize: UIntPtr;
      var AOldHeaders: PImageNtHeaders);
    function PerformBaseRelocation(const ADelta: IntPtr): Boolean;
    function GetHeaderDictionary(const AIdx: Integer): PImageDataDirectory; inline;
    function GetRealSectionSize(const ASection: PImageSectionHeader): UIntPtr;
    procedure BuildImportTable;
    procedure FinalizeSections;
    procedure FinalizeSection(var ASectionData: TSectionFinalizeData);
    procedure ExecuteTls;
    {$IFDEF WIN64}
    procedure FreePointerList(const AHead: PPointerList);
    {$ENDIF}
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const AData: TBytes); overload;
    constructor Create(const AResourceName: String); overload;
    destructor Destroy; override;
    procedure FreeMemoryOnly;

    function GetProcAddress(const AName: PAnsiChar): Pointer;
  end;

implementation

uses
  System.Win.Crtl,
  System.Types,
  System.Classes,
  System.Generics.Collections;

const
  {$IFDEF WIN32}
  HOST_MACHINE = IMAGE_FILE_MACHINE_I386;
  {$ELSE}
  HOST_MACHINE = IMAGE_FILE_MACHINE_AMD64;
  {$ENDIF}

const
  PROTECTION_FLAGS: array [Boolean, Boolean, Boolean] of DWORD = (
   ((PAGE_NOACCESS, PAGE_WRITECOPY),
    (PAGE_READONLY, PAGE_READWRITE)),
   ((PAGE_EXECUTE, PAGE_EXECUTE_WRITECOPY),
    (PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE)));

type
  PUIntPtr = ^UIntPtr;
  PFarProc = ^TFarProc;
  PImageTLSCallback = ^TImageTLSCallback;

{ Missing from Winapi.Window.pas }

const
  IMAGE_REL_BASED_HIGHLOW = 3;
  IMAGE_REL_BASED_DIR64   = 10;

type
  TImageBaseRelocation = record
  public
    VirtualAddress: DWORD;
    SizeOfBlock: DWORD;
  end;
  PImageBaseRelocation = ^TImageBaseRelocation;

function ImageSnapByOrdinal(const AOrdinal: UIntPtr): Boolean; inline;
begin
  {$IFDEF WIN64}
  Result := ((AOrdinal and IMAGE_ORDINAL_FLAG64) <> 0);
  {$ELSE}
  Result := ((AOrdinal and IMAGE_ORDINAL_FLAG32) <> 0);
  {$ENDIF}
end;

function ImageOrdinal(const AOrdinal: UIntPtr): UIntPtr; inline;
begin
  Result := AOrdinal and $FFFF;
end;

{ TMemoryModule }

class function TMemoryModule.AlignAddressDown(const AAddress: Pointer;
  const AAlignment: UIntPtr): Pointer;
begin
  Result := Pointer(AlignValueDown(UIntPtr(AAddress), AAlignment));
end;

class function TMemoryModule.AlignValueDown(const AValue,
  AAlignment: UIntPtr): UIntPtr;
begin
  Result := AValue and not (AAlignment - 1);
end;

class function TMemoryModule.AlignValueUp(const AValue,
  AAlignment: UIntPtr): UIntPtr;
begin
  Result := (AValue + AAlignment - 1) and not (AAlignment - 1);
end;

procedure TMemoryModule.BuildImportTable;
begin
  var CodeBase := PByte(FCodeBase);
  var Directory := GetHeaderDictionary(IMAGE_DIRECTORY_ENTRY_IMPORT);
  if (Directory.Size = 0) then
    Exit;

  var ImportDesc := PImageImportDescriptor(CodeBase + Directory.VirtualAddress);
  while (not IsBadReadPtr(ImportDesc, SizeOf(TImageImportDescriptor)))
    and (ImportDesc.Name <> 0) do
  begin
    var Handle := LoadLibraryA(LPCSTR(CodeBase + ImportDesc.Name));
    if (Handle = 0) then
      RaiseLastOSError(ERROR_MOD_NOT_FOUND);

    FModules := FModules + [Handle];

    var ThunkRef: PUIntPtr;
    if (ImportDesc.OriginalFirstThunk <> 0) then
      ThunkRef := PUIntPtr(CodeBase + ImportDesc.OriginalFirstThunk)
    else
      ThunkRef := PUIntPtr(CodeBase + ImportDesc.FirstThunk);

    var FuncRef := PFarProc(CodeBase + ImportDesc.FirstThunk);

    while (ThunkRef^ <> 0) do
    begin
      if (ImageSnapByOrdinal(ThunkRef^)) then
        FuncRef^ := Winapi.Windows.GetProcAddress(Handle, LPCSTR(ImageOrdinal(ThunkRef^)))
      else
      begin
        var ThunkData := PImageImportByName(CodeBase + ThunkRef^);
        FuncRef^ := Winapi.Windows.GetProcAddress(Handle, LPCSTR(@ThunkData.Name));
      end;

      if (FuncRef^ = nil) then
        RaiseLastOSError(ERROR_PROC_NOT_FOUND);

      Inc(ThunkRef);
      Inc(FuncRef);
    end;
    Inc(ImportDesc);
  end;
end;

class procedure TMemoryModule.CheckSize(const ASize, AExpected: IntPtr);
begin
  if (ASize < AExpected) then
    RaiseLastOSError(ERROR_INVALID_DATA);
end;

procedure TMemoryModule.CopySections(const AData: PByte; const ASize: UIntPtr;
  var AOldHeaders: PImageNtHeaders);
begin
  var CodeBase := PByte(FCodeBase);
  var Section := ImageFirstSection(FHeaders);
  for var I := 0 to Smallint(FHeaders.FileHeader.NumberOfSections) - 1 do
  begin
    var Dest: PByte;
    if (Section.SizeOfRawData = 0) then
    begin
      var SectionSize := AOldHeaders.OptionalHeader.SectionAlignment;
      if (SectionSize > 0) then
      begin
        Dest := VirtualAlloc(CodeBase + Section.VirtualAddress,
          SectionSize, MEM_COMMIT, PAGE_READWRITE);
        if (Dest = nil) then
          RaiseLastOSError(ERROR_OUTOFMEMORY);

        Dest := CodeBase + Section.VirtualAddress;
        Section.Misc.PhysicalAddress := UIntPtr(Dest) and $FFFFFFFF;
        FillChar(Dest^, SectionSize, 0);
      end;

      Inc(Section);
      Continue;
    end;

    CheckSize(ASize, Section.PointerToRawData + Section.SizeOfRawData);

    Dest := VirtualAlloc(CodeBase + Section.VirtualAddress,
      Section.SizeOfRawData, MEM_COMMIT, PAGE_READWRITE);
    if (Dest = nil) then
      RaiseLastOSError(ERROR_OUTOFMEMORY);

    Dest := CodeBase + Section.VirtualAddress;
    Move(AData[Section.PointerToRawData], Dest^, Section.SizeOfRawData);
    Section.Misc.PhysicalAddress := UIntPtr(Dest) and $FFFFFFFF;

    Inc(Section);
  end;
end;

constructor TMemoryModule.Create(const AResourceName: String);
begin
  var Data: TBytes;
  var Stream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    SetLength(Data, Stream.Size);
    Stream.ReadBuffer(Data, Length(Data));
  finally
    Stream.Free;
  end;
  Create(Data);
end;

destructor TMemoryModule.Destroy;
begin
  if (not FFreeMemoryOnly) then
  begin
    if (FInitialized) then
    begin
      var DllEntry: TDllEntryProc;
      @DllEntry := Pointer(PByte(FCodeBase) + FHeaders.OptionalHeader.AddressOfEntryPoint);
      DllEntry(THandle(FCodeBase), DLL_PROCESS_DETACH, nil);
    end;

    for var Module in FModules do
    begin
      if (Module <> 0) then
        Winapi.Windows.FreeLibrary(Module);
    end;

    if (FCodeBase <> nil) then
      VirtualFree(FCodeBase, 0, MEM_RELEASE);
  end;

  {$IFDEF WIN64}
  FreePointerList(FBlockedMemory);
  {$ENDIF}
  inherited;
end;

procedure TMemoryModule.ExecuteTls;
begin
  var Directory := GetHeaderDictionary(IMAGE_DIRECTORY_ENTRY_TLS);
  if (Directory.VirtualAddress = 0) then
    Exit;

  var CodeBase := PByte(FCodeBase);
  var Tls := PIMAGE_TLS_DIRECTORY(CodeBase + Directory.VirtualAddress);
  var Callback := PImageTLSCallback(Tls.AddressOfCallBacks);
  if Assigned(Callback) then
  begin
    while (Assigned(Callback^)) do
    begin
      Callback^(CodeBase, DLL_PROCESS_ATTACH, nil);
      Inc(Callback);
    end;
  end;
end;

procedure TMemoryModule.FinalizeSection(var ASectionData: TSectionFinalizeData);
begin
  if (ASectionData.Size = 0) then
    Exit;

  if ((ASectionData.Characteristics and IMAGE_SCN_MEM_DISCARDABLE) <> 0) then
  begin
    if (ASectionData.Address = ASectionData.AlignedAddress) and
      (ASectionData.Last or (FHeaders.OptionalHeader.SectionAlignment = FPageSize)
      or ((ASectionData.Size mod FPageSize) = 0))
    then
      VirtualFree(ASectionData.Address, ASectionData.Size, MEM_DECOMMIT);

    Exit;
  end;

  var Executable := ((ASectionData.Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0);
  var Readable := ((ASectionData.Characteristics and IMAGE_SCN_MEM_READ) <> 0);
  var Writeable := ((ASectionData.Characteristics and IMAGE_SCN_MEM_WRITE) <> 0);
  var Protect := PROTECTION_FLAGS[Executable, Readable, Writeable];

  if ((ASectionData.Characteristics and IMAGE_SCN_MEM_NOT_CACHED) <> 0) then
    Protect := Protect or PAGE_NOCACHE;

  var OldProtect: DWORD;
  if (not VirtualProtect(ASectionData.Address, ASectionData.Size, Protect, OldProtect)) then
    RaiseLastOSError(ERROR_NOACCESS);
end;

procedure TMemoryModule.FinalizeSections;
begin
  var Section := ImageFirstSection(FHeaders);
  var ImageOffset: UIntPtr;
  {$IFDEF WIN64}
  ImageOffset := FHeaders.OptionalHeader.ImageBase and $FFFFFFFF00000000;
  {$ELSE}
  ImageOffset := 0;
  {$ENDIF}

  var SectionData: TSectionFinalizeData;
  SectionData.Address := Pointer(Section.Misc.PhysicalAddress or ImageOffset);
  SectionData.AlignedAddress := AlignAddressDown(SectionData.Address, FPageSize);
  SectionData.Size := GetRealSectionSize(Section);
  SectionData.Characteristics := Section.Characteristics;
  SectionData.Last := False;
  Inc(Section);

  for var I := 1 to Smallint(FHeaders.FileHeader.NumberOfSections) - 1 do
  begin
    var SectionAddress := Pointer(Section.Misc.PhysicalAddress or ImageOffset);
    var AlignedAddress := AlignAddressDown(SectionAddress, FPageSize);
    var SectionSize := GetRealSectionSize(Section);

    if (SectionData.AlignedAddress = AlignedAddress)
      or ((UIntPtr(SectionData.Address) + SectionData.Size) > UIntPtr(AlignedAddress)) then
    begin
      if ((Section.Characteristics and IMAGE_SCN_MEM_DISCARDABLE) = 0)
        or ((SectionData.Characteristics and IMAGE_SCN_MEM_DISCARDABLE) = 0)
      then
        SectionData.Characteristics := (SectionData.Characteristics or Section.Characteristics) and not IMAGE_SCN_MEM_DISCARDABLE
      else
        SectionData.Characteristics := (SectionData.Characteristics or Section.Characteristics);

      SectionData.Size := UIntPtr(SectionAddress) + UIntPtr(SectionSize) - UIntPtr(SectionData.Address);
      Inc(Section);
      Continue;
    end;

    FinalizeSection(SectionData);

    SectionData.Address := SectionAddress;
    SectionData.AlignedAddress := AlignedAddress;
    SectionData.Size := SectionSize;
    SectionData.Characteristics := Section.Characteristics;

    Inc(Section);
  end;

  SectionData.Last := True;
  FinalizeSection(SectionData);
end;

procedure TMemoryModule.FreeMemoryOnly;
begin
  if (Self <> nil) then
  begin
    FFreeMemoryOnly := True;
    Destroy;
  end;
end;

{$IFDEF WIN64}
procedure TMemoryModule.FreePointerList(const AHead: PPointerList);
begin
  var Node := AHead;
  while Assigned(Node) do
  begin
    VirtualFree(Node.Address, 0, MEM_RELEASE);
    var Next := Node.Next;
    FreeMem(Node);
    Node := Next;
  end;
end;
{$ENDIF}

function TMemoryModule.GetHeaderDictionary(
  const AIdx: Integer): PImageDataDirectory;
begin
  Result := @FHeaders.OptionalHeader.DataDirectory[AIdx];
end;

function TMemoryModule.GetProcAddress(const AName: PAnsiChar): Pointer;
begin
  var CodeBase := PByte(FCodeBase);
  var Directory := GetHeaderDictionary(IMAGE_DIRECTORY_ENTRY_EXPORT);
  if (Directory.Size = 0) then
    RaiseLastOSError(ERROR_PROC_NOT_FOUND);

  var ExportDir := PImageExportDirectory(CodeBase + Directory.VirtualAddress);
  if (ExportDir.NumberOfNames = 0) or (ExportDir.NumberOfFunctions = 0) then
    RaiseLastOSError(ERROR_PROC_NOT_FOUND);

  var Idx: DWORD;
  if (HiWord(UIntPtr(AName)) = 0) then
  begin
    if (LoWord(UIntPtr(AName)) < ExportDir.Base) then
      RaiseLastOSError(ERROR_PROC_NOT_FOUND);

    Idx := LoWord(UIntPtr(AName)) - ExportDir.Base;
  end
  else
  begin
    if (FNameExportsTable = nil) then
    begin
      var NameRef := PDWORD(CodeBase + ExportDir.AddressOfNames);
      var Ordinal := PWORD(CodeBase + ExportDir.AddressOfNameOrdinals);
      SetLength(FNameExportsTable, ExportDir.NumberOfNames);

      for var I := 0 to Length(FNameExportsTable) - 1 do
      begin
        FNameExportsTable[I].Name := Pointer(CodeBase + NameRef^);
        FNameExportsTable[I].Idx := Ordinal^;

        Inc(NameRef);
        Inc(Ordinal);
      end;

      if (FComparer = nil) then
      begin
        FComparer := TComparer<TExportNameEntry>.Construct(
          function(const ALeft, ARight: TExportNameEntry): Integer
          begin
            Result := strcmp(ALeft.Name, ARight.Name);
          end);
      end;

      TArray.Sort<TExportNameEntry>(FNameExportsTable, FComparer);
    end;

    var ToFind: TExportNameEntry;
    ToFind.Name := AName;
    var FoundIndex: NativeInt;

    if (not (TArray.BinarySearch<TExportNameEntry>(FNameExportsTable, ToFind,
      FoundIndex, FComparer)))
    then
      RaiseLastOSError(ERROR_PROC_NOT_FOUND);

    Idx := FNameExportsTable[FoundIndex].Idx;
  end;

  if (Idx > ExportDir.NumberOfFunctions) then
    RaiseLastOSError(ERROR_PROC_NOT_FOUND);

  Result := Pointer(CodeBase + PDWORD(CodeBase + ExportDir.AddressOfFunctions + (Idx * 4))^);
end;

function TMemoryModule.GetRealSectionSize(
  const ASection: PImageSectionHeader): UIntPtr;
begin
  Result := ASection.SizeOfRawData;
  if (Result = 0) then
  begin
    if ((ASection.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0) then
      Result := FHeaders.OptionalHeader.SizeOfInitializedData
    else if ((ASection.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA) <> 0) then
      Result := FHeaders.OptionalHeader.SizeOfUninitializedData;
  end;
end;

class function TMemoryModule.ImageFirstSection(
  const AHeader: PImageNtHeaders): PImageSectionHeader;
begin
  var Section := UIntPtr(@AHeader.OptionalHeader) + AHeader.FileHeader.SizeOfOptionalHeader;
  Result := Pointer(Section);
end;

class function TMemoryModule.OffsetPointer(const AData: Pointer;
  const AOffset: IntPtr): Pointer;
begin
  Result := Pointer(IntPtr(AData) + AOffset);
end;

function TMemoryModule.PerformBaseRelocation(const ADelta: IntPtr): Boolean;
begin
  var CodeBase := PByte(FCodeBase);
  var Directory := GetHeaderDictionary(IMAGE_DIRECTORY_ENTRY_BASERELOC);
  if (Directory.Size = 0) then
    Exit(ADelta = 0);

  var Relocation := PImageBaseRelocation(CodeBase + Directory.VirtualAddress);
  while (Relocation.VirtualAddress > 0) do
  begin
    var Dest := CodeBase + Relocation.VirtualAddress;
    var RelInfo := PSmallint(OffsetPointer(Relocation, SizeOf(TImageBaseRelocation)));
    for var I := 0 to Integer(Relocation.SizeOfBlock - SizeOf(TImageBaseRelocation)) div 2 - 1 do
    begin
      var Typ: Integer := RelInfo^ shr 12;
      var Offset: Integer := RelInfo^ and $FFF;

      case Typ of
        IMAGE_REL_BASED_HIGHLOW:
          begin
            var PatchAddrHL := PDWORD(Dest + Offset);
            Inc(PatchAddrHL^, ADelta);
          end;

        {$IFDEF WIN64}
        IMAGE_REL_BASED_DIR64:
          begin
            var PatchAddr64 := PULONGLONG(Dest + Offset);
            Inc(PatchAddr64^, ADelta);
          end;
        {$ENDIF}
      end;
      Inc(RelInfo);
    end;

    Relocation := OffsetPointer(Relocation, Relocation.SizeOfBlock);
  end;
  Result := True;
end;

constructor TMemoryModule.Create(const AData: TBytes);
begin
  inherited Create;
//  FData := AData;

  var Size := Length(AData);
  CheckSize(Size, SizeOf(TImageDosHeader));

  var DosHeader := PImageDosHeader(AData);
  if (DosHeader.e_magic <> IMAGE_DOS_SIGNATURE) then
    RaiseLastOSError(ERROR_BAD_EXE_FORMAT);

  CheckSize(Size, DosHeader._lfanew + SizeOf(TImageNtHeaders));

  var OldHeader := PImageNtHeaders(@AData[DosHeader._lfanew]);
  if (OldHeader.Signature <> IMAGE_NT_SIGNATURE) then
    RaiseLastOSError(ERROR_BAD_EXE_FORMAT);

  if (OldHeader.FileHeader.Machine <> HOST_MACHINE) then
    RaiseLastOSError(ERROR_BAD_EXE_FORMAT);

  if (Odd(OldHeader.OptionalHeader.SectionAlignment)) then
    RaiseLastOSError(ERROR_BAD_EXE_FORMAT);

  if ((OldHeader.FileHeader.Characteristics and IMAGE_FILE_DLL) = 0) then
    RaiseLastOSError(ERROR_BAD_EXE_FORMAT);

  var Section := ImageFirstSection(OldHeader);
  var OptionalSectionSize := OldHeader.OptionalHeader.SectionAlignment;
  var LastSectionEnd: UIntPtr := 0;

  for var I := 0 to Smallint(OldHeader.FileHeader.NumberOfSections) - 1 do
  begin
    var EndOfSection: UIntPtr;
    if (Section.SizeOfRawData = 0) then
      EndOfSection := Section.VirtualAddress + OptionalSectionSize
    else
      EndOfSection := Section.VirtualAddress + Section.SizeOfRawData;

    if (EndOfSection > LastSectionEnd) then
      LastSectionEnd := EndOfSection;

    Inc(Section);
  end;

  var SysInfo: TSystemInfo;
  GetNativeSystemInfo(SysInfo);
  var AlignedImageSize := AlignValueUp(OldHeader.OptionalHeader.SizeOfImage, SysInfo.dwPageSize);
  if (AlignedImageSize <> AlignValueUp(LastSectionEnd, SysInfo.dwPageSize)) then
    RaiseLastOSError(ERROR_BAD_EXE_FORMAT);

  FCodeBase := VirtualAlloc(Pointer(OldHeader.OptionalHeader.ImageBase),
    AlignedImageSize, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);

  if (FCodeBase = nil) then
  begin
    FCodeBase := VirtualAlloc(nil, AlignedImageSize, MEM_RESERVE or MEM_COMMIT,
      PAGE_READWRITE);

    if (FCodeBase = nil) then
      RaiseLastOSError(ERROR_OUTOFMEMORY);
  end;

  {$IFDEF WIN64}
  while ((UIntPtr(FCodeBase) shr 32) < ((UIntPtr(FCodeBase) + AlignedImageSize) shr 32)) do
  begin
    var Node: PPointerList;
    GetMem(Node, SizeOf(TPointerList));
    Node.Next := FBlockedMemory;
    Node.Address := FCodeBase;
    FBlockedMemory := Node;

    FCodeBase := VirtualAlloc(nil, AlignedImageSize, MEM_RESERVE or MEM_COMMIT,
      PAGE_READWRITE);

    if (FCodeBase = nil) then
      RaiseLastOSError(ERROR_OUTOFMEMORY);
  end;
  {$ENDIF}

  FPageSize := SysInfo.dwPageSize;

  CheckSize(Size, OldHeader.OptionalHeader.SizeOfHeaders);
  var Headers := VirtualAlloc(FCodeBase, OldHeader.OptionalHeader.SizeOfHeaders,
    MEM_COMMIT, PAGE_READWRITE);

  Move(DosHeader^, Headers^, OldHeader.OptionalHeader.SizeOfHeaders);
  FHeaders := Pointer(PByte(Headers) + DosHeader._lfanew);
  FHeaders.OptionalHeader.ImageBase := UIntPtr(FCodeBase);

  CopySections(PByte(AData), Size, OldHeader);

  var LocationDelta: IntPtr := FHeaders.OptionalHeader.ImageBase - OldHeader.OptionalHeader.ImageBase;
  if (LocationDelta <> 0) then
    FIsRelocated := PerformBaseRelocation(LocationDelta)
  else
    FIsRelocated := True;

  BuildImportTable;
  FinalizeSections;
  ExecuteTls;

  if (FHeaders.OptionalHeader.AddressOfEntryPoint <> 0) then
  begin
    var DllEntry: TDllEntryProc;
    @DlLEntry := PByte(FCodeBase) + FHeaders.OptionalHeader.AddressOfEntryPoint;
    var SuccessFull := DllEntry(THandle(FCodeBase), DLL_PROCESS_ATTACH, nil);
    if (not SuccessFull) then
      RaiseLastOSError(ERROR_DLL_INIT_FAILED);
  end;

  FInitialized := True;
end;

end.
