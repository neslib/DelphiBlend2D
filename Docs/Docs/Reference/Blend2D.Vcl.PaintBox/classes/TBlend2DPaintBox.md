# TBlend2DPaintBox

A paint box that uses Blend2D for drawing. 

## Definition

Unit: [Blend2D.Vcl.PaintBox](../index.md)

``` delphi
type TBlend2DPaintBox = class(TGraphicControl) ... end
```

### Inheritance

**Inherits:** TGraphicControl

## Remarks

To be able to install a package with this control, you *must* copy "blend2d_win32.dll" to your package output directory (usually c:\Users\Public\Documents\Embarcadero\Studio\<version>\Bpl\) </version>

## Properties

| Name | Description |
| --- | --- |
| [Align](#Align) |  |
| [Anchors](#Anchors) |  |
| [Color](#Color) |  |
| [Constraints](#Constraints) |  |
| [DragCursor](#DragCursor) |  |
| [DragKind](#DragKind) |  |
| [DragMode](#DragMode) |  |
| [Enabled](#Enabled) |  |
| [Font](#Font) |  |
| [LowResolution](#LowResolution) | Whether to use a low-resolution buffer on high-DPI (eg. Retina) displays. |
| [ParentColor](#ParentColor) |  |
| [ParentFont](#ParentFont) |  |
| [ParentShowHint](#ParentShowHint) |  |
| [PopupMenu](#PopupMenu) |  |
| [ShowHint](#ShowHint) |  |
| [Touch](#Touch) |  |
| [Visible](#Visible) | |

## Events

| Name | Description |
| --- | --- |
| [OnClick](#OnClick) |  |
| [OnContextPopup](#OnContextPopup) |  |
| [OnDblClick](#OnDblClick) |  |
| [OnDragDrop](#OnDragDrop) |  |
| [OnDragOver](#OnDragOver) |  |
| [OnEndDock](#OnEndDock) |  |
| [OnEndDrag](#OnEndDrag) |  |
| [OnGesture](#OnGesture) |  |
| [OnMouseActivate](#OnMouseActivate) |  |
| [OnMouseDown](#OnMouseDown) |  |
| [OnMouseEnter](#OnMouseEnter) |  |
| [OnMouseLeave](#OnMouseLeave) |  |
| [OnMouseMove](#OnMouseMove) |  |
| [OnMouseUp](#OnMouseUp) |  |
| [OnPaint](#OnPaint) |  |
| [OnStartDock](#OnStartDock) |  |
| [OnStartDrag](#OnStartDrag) | |

## Constructors

| Name | Description |
| --- | --- |
| [Create](#Create) |  |
| [Destroy](#Destroy) | |

## Methods

| Name | Description |
| --- | --- |
| [ChangeScale](#ChangeScale) |  |
| [Paint](#Paint) | |

## Property Descriptions

###  :material-alpha-p-circle:{ title="Property" } Align {#Align}

`#!delphi property Align: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Anchors {#Anchors}

`#!delphi property Anchors: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Color {#Color}

`#!delphi property Color: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Constraints {#Constraints}

`#!delphi property Constraints: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } DragCursor {#DragCursor}

`#!delphi property DragCursor: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } DragKind {#DragKind}

`#!delphi property DragKind: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } DragMode {#DragMode}

`#!delphi property DragMode: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Enabled {#Enabled}

`#!delphi property Enabled: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Font {#Font}

`#!delphi property Font: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } LowResolution {#LowResolution}

Whether to use a low-resolution buffer on high-DPI (eg. Retina) displays.

When set to True, the drawing canvas will be in logical units instead of physical units. This will increase performance but result in lower quality (less sharp) output.

When set to False (the default), the drawing canvas will match the units of the display, resulting a sharp high-quality output at the cost of a decrease in performance.

On non-high-DPI displays, this property has no effect. 

`#!delphi property LowResolution: Boolean read FLowResolution write SetLowResolution default False`

**Type: **`#!delphi Boolean`

---

###  :material-alpha-p-circle:{ title="Property" } ParentColor {#ParentColor}

`#!delphi property ParentColor: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } ParentFont {#ParentFont}

`#!delphi property ParentFont: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } ParentShowHint {#ParentShowHint}

`#!delphi property ParentShowHint: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } PopupMenu {#PopupMenu}

`#!delphi property PopupMenu: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } ShowHint {#ShowHint}

`#!delphi property ShowHint: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Touch {#Touch}

`#!delphi property Touch: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Visible {#Visible}

`#!delphi property Visible: `

**Type: **`#!delphi `

---

## Event Descriptions

###  :material-alpha-e-circle:{ title="Event" } OnClick {#OnClick}

`#!delphi property OnClick: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnContextPopup {#OnContextPopup}

`#!delphi property OnContextPopup: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnDblClick {#OnDblClick}

`#!delphi property OnDblClick: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnDragDrop {#OnDragDrop}

`#!delphi property OnDragDrop: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnDragOver {#OnDragOver}

`#!delphi property OnDragOver: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnEndDock {#OnEndDock}

`#!delphi property OnEndDock: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnEndDrag {#OnEndDrag}

`#!delphi property OnEndDrag: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnGesture {#OnGesture}

`#!delphi property OnGesture: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnMouseActivate {#OnMouseActivate}

`#!delphi property OnMouseActivate: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnMouseDown {#OnMouseDown}

`#!delphi property OnMouseDown: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnMouseEnter {#OnMouseEnter}

`#!delphi property OnMouseEnter: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnMouseLeave {#OnMouseLeave}

`#!delphi property OnMouseLeave: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnMouseMove {#OnMouseMove}

`#!delphi property OnMouseMove: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnMouseUp {#OnMouseUp}

`#!delphi property OnMouseUp: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnPaint {#OnPaint}

`#!delphi property OnPaint: TBlend2DPaintEvent read FOnPaint write FOnPaint`

**Type: **`#!delphi TBlend2DPaintEvent`

---

###  :material-alpha-e-circle:{ title="Event" } OnStartDock {#OnStartDock}

`#!delphi property OnStartDock: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnStartDrag {#OnStartDrag}

`#!delphi property OnStartDrag: `

**Type: **`#!delphi `

---

## Constructor Descriptions

###  :material-decagram:{ title="Constructor" } `#!delphi Create` {#Create}

`#!delphi constructor Create(AOwner: TComponent); override`
#### Parameters

**`AOwner`**: `#!delphi TComponent`

---

###  :material-delete:{ title="Destructor" } `#!delphi Destroy` {#Destroy}

`#!delphi destructor Destroy; override`

---

## Method Descriptions

###  :material-alpha-m-circle:{ title="Method" } ChangeScale(Integer, Integer, Boolean) {#ChangeScale}

`#!delphi procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override`
#### Parameters

**`M`**: `#!delphi Integer`

**`D`**: `#!delphi Integer`

**`isDpiChange`**: `#!delphi Boolean`

---

###  :material-alpha-m-circle:{ title="Method" } Paint {#Paint}

`#!delphi procedure Paint; override`

---

