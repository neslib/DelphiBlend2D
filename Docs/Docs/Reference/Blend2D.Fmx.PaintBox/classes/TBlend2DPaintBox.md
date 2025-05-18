# TBlend2DPaintBox

A paint box that uses Blend2D for drawing. To be able to install a package with this control, you *must* copy "blend2d_win32.dll" to your package output directory (usually c:\Users\Public\Documents\Embarcadero\Studio\<version>\Bpl\) </version>

## Definition

Unit: [Blend2D.Fmx.PaintBox](../index.md)

``` delphi
type TBlend2DPaintBox = class(TControl) ... end
```

### Inheritance

**Inherits:** TControl

## Properties

| Name | Description |
| --- | --- |
| [Align](#Align) |  |
| [Anchors](#Anchors) |  |
| [ClipChildren](#ClipChildren) |  |
| [ClipParent](#ClipParent) |  |
| [Cursor](#Cursor) |  |
| [DragMode](#DragMode) |  |
| [Enabled](#Enabled) |  |
| [EnableDragHighlight](#EnableDragHighlight) |  |
| [Height](#Height) |  |
| [HitTest](#HitTest) |  |
| [Locked](#Locked) |  |
| [LowResolution](#LowResolution) | Whether to use a low-resolution buffer on high-DPI (eg. Retina) displays. |
| [Margins](#Margins) |  |
| [Opacity](#Opacity) |  |
| [Padding](#Padding) |  |
| [PopupMenu](#PopupMenu) |  |
| [Position](#Position) |  |
| [RotationAngle](#RotationAngle) |  |
| [RotationCenter](#RotationCenter) |  |
| [Scale](#Scale) |  |
| [Size](#Size) |  |
| [Visible](#Visible) |  |
| [Width](#Width) | |

## Events

| Name | Description |
| --- | --- |
| [OnClick](#OnClick) |  |
| [OnDblClick](#OnDblClick) |  |
| [OnDragDrop](#OnDragDrop) |  |
| [OnDragEnd](#OnDragEnd) |  |
| [OnDragEnter](#OnDragEnter) |  |
| [OnDragLeave](#OnDragLeave) |  |
| [OnDragOver](#OnDragOver) |  |
| [OnMouseDown](#OnMouseDown) |  |
| [OnMouseEnter](#OnMouseEnter) |  |
| [OnMouseLeave](#OnMouseLeave) |  |
| [OnMouseMove](#OnMouseMove) |  |
| [OnMouseUp](#OnMouseUp) |  |
| [OnMouseWheel](#OnMouseWheel) |  |
| [OnPaint](#OnPaint) |  |
| [OnPainting](#OnPainting) |  |
| [OnResize](#OnResize) |  |
| [OnResized](#OnResized) | |

## Constructors

| Name | Description |
| --- | --- |
| [Create](#Create) |  |
| [Destroy](#Destroy) | |

## Methods

| Name | Description |
| --- | --- |
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

###  :material-alpha-p-circle:{ title="Property" } ClipChildren {#ClipChildren}

`#!delphi property ClipChildren:  default False`

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } ClipParent {#ClipParent}

`#!delphi property ClipParent:  default False`

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Cursor {#Cursor}

`#!delphi property Cursor:  default crDefault`

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } DragMode {#DragMode}

`#!delphi property DragMode:  default TDragMode.dmManual`

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Enabled {#Enabled}

`#!delphi property Enabled:  default True`

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } EnableDragHighlight {#EnableDragHighlight}

`#!delphi property EnableDragHighlight:  default True`

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Height {#Height}

`#!delphi property Height: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } HitTest {#HitTest}

`#!delphi property HitTest:  default True`

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Locked {#Locked}

`#!delphi property Locked:  default False`

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

###  :material-alpha-p-circle:{ title="Property" } Margins {#Margins}

`#!delphi property Margins: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Opacity {#Opacity}

`#!delphi property Opacity: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Padding {#Padding}

`#!delphi property Padding: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } PopupMenu {#PopupMenu}

`#!delphi property PopupMenu: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Position {#Position}

`#!delphi property Position: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } RotationAngle {#RotationAngle}

`#!delphi property RotationAngle: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } RotationCenter {#RotationCenter}

`#!delphi property RotationCenter: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Scale {#Scale}

`#!delphi property Scale: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Size {#Size}

`#!delphi property Size: `

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Visible {#Visible}

`#!delphi property Visible:  default True`

**Type: **`#!delphi `

---

###  :material-alpha-p-circle:{ title="Property" } Width {#Width}

`#!delphi property Width: `

**Type: **`#!delphi `

---

## Event Descriptions

###  :material-alpha-e-circle:{ title="Event" } OnClick {#OnClick}

`#!delphi property OnClick: `

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

###  :material-alpha-e-circle:{ title="Event" } OnDragEnd {#OnDragEnd}

`#!delphi property OnDragEnd: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnDragEnter {#OnDragEnter}

`#!delphi property OnDragEnter: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnDragLeave {#OnDragLeave}

`#!delphi property OnDragLeave: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnDragOver {#OnDragOver}

`#!delphi property OnDragOver: `

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

###  :material-alpha-e-circle:{ title="Event" } OnMouseWheel {#OnMouseWheel}

`#!delphi property OnMouseWheel: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnPaint {#OnPaint}

`#!delphi property OnPaint: TBlend2DPaintEvent read FOnPaint write FOnPaint`

**Type: **`#!delphi TBlend2DPaintEvent`

---

###  :material-alpha-e-circle:{ title="Event" } OnPainting {#OnPainting}

`#!delphi property OnPainting: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnResize {#OnResize}

`#!delphi property OnResize: `

**Type: **`#!delphi `

---

###  :material-alpha-e-circle:{ title="Event" } OnResized {#OnResized}

`#!delphi property OnResized: `

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

###  :material-alpha-m-circle:{ title="Method" } Paint {#Paint}

`#!delphi procedure Paint; override`

---

