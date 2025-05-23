# Multithreaded Rendering

Blend2D's rendering context can utilize multiple threads that render to the same target image. This section elaborates on various aspects of this feature and includes useful tips to avoid problems that naturally arise with asynchronous rendering, i.e. the rendering context doesn't change the content of the target image immediately. Instead, it serializes the work to be done into batches that are then executed by worker threads when ready.

Since most of the software-based 2D rendering engines are synchronous some users may expect that calling a function like `FillRect` would immediately change relevant pixels of the target image and then return. This is true for many 2D libraries like AGG, Cairo, Qt, and Skia when a software accelerated rendering is used. We can describe such rendering mode as synchronous as it blocks until each operation is completed. However, this assumption is not true with Blend2D's multi-threaded rendering context implementation. Such implementations will be referred to as asynchronous, because in that case the rendering context only serializes the requested operations to be completed later in batches.

!!! note

    The multi-threaded rendering engine implementation is still experimental. We believe that its performance can be improved in the future, especially its synchronization and resource sharing parts. We definitely need more feedback from Blend2D users to improve the implementation even further.

## Initialization & Finalization

By default a single-threaded (and thus synchronous rendering context) is created unless a filled `TBLContextCreateInfo` is passed to the rendering context's constructor or `Start` method. A `ThreadCount` member specifies how many threads to use. At the moment there are no defaults or heuristics in the library so users must carefully use a value that makes sense for their workloads. In general 2 to 4 threads should be suitable for most workloads including real-time rendering to 4K frame-buffers. If the framebuffer is relatively small (e.g. 200x200 pixels) it's recommended to either use single-threaded rendering by default or to at least benchmark whether it has any benefits before switching to multi-threaded rendering.

The initialization may look like this:

```Delphi
var Image := TBLImage.Create(500, 500, TBLFormat.Prgb32);

{ The best is to use Reset so the record content is zeroed.
  Blend2D guarantees that any future additions will work well with
  zeros by default. }
var CreateInfo: TBLContextCreateInfo;
CreateInfo.Reset;

{ Configure the number of threads to use. }
CreateInfo.ThreadCount := SomeThreadCount;

{ Create the rendering context.

  Either use constructor like TBLContext.Create(Image, CreateInfo) or
  TBLContext.Start. This operation shouldn't fail by default unless there
  is not enough memory to allocate the context and some helper data.

  In addition, if the internal thread-pool is exhausted the rendering
  context would still be asynchronous, but would only use the user
  thread. This behavior can be overridden by changing initialization
  flags in 'CreateInfo'. }
var Context: TBLContext;
Context.Start(Image, CreateInfo);

{ ... render something ...

  Use either Context.Flush or Context.Finish to synchronize }
Context.Finish;

{ Now you can use the content of Image. }
Image.WriteToFile('blMultithreadedExampleInit.png');
```

It can be seen from the previous example that Blend2D provides the same API for both synchronous and asynchronous rendering, only the initialization phase is different. Additionally, one important detail is the significance of the `Finish` method. For the synchronous case it's a relatively cheap call that only releases some resources and detaches the rendering context from the image. However, in the asynchronous case, `Finish` actually synchronizes first. The rendering will start at that point and the execution will block until it's done. Only after it's done the resources can be released and the function can return.

## Synchronization

There are two functions that can be used to require synchronization of the rendering context, which means flushing the render queue and waiting for completion:

* `Flush([TBLContextFlushFlag.Sync])` - flushes all pending operations and waits for their completion
* `Finish` - this call does the same as `Flush([TBLContextFlushFlag.Sync])` in addition to detaching the rendering context from the target image after there are no more pending operations

Both `Flush` and `Finish` calls are blocking, which means that they would only return after all threads finish their work. The importance of flush can be illustrated on the next example:

```Delphi
var CreateInfo: TBLContextCreateInfo;
CreateInfo.Reset;
CreateInfo.ThreadCount := 2;

var Image := TBLImage.Create(500, 500, TBLFormat.Prgb32);
var Context := TBLContext.Create(Image, CreateInfo);

Context.FillAll(TAlphaColors.Black);
Context.FillRoundRect(BLRoundRect(25, 25, 450, 450, 50), TAlphaColors.White);

{ This won't work! The content of the pixels is undefined at the moment. }
Image.WriteToFile('blMultithreadedExampleSyncWrong.png');

{ This actually flushes all render calls and synchronizes. }
Context.Flush([TBLContextFlushFlag.Sync]);

{ Now the image contains everything we have asked for. }
Image.WriteToFile('blMultithreadedExampleSyncRight.png');

{ This would synchronize if there were any pending operations, but in
  our case it would just detach the rendering context from the image
  and return all threads to the thread pool (which is a cheap operation). }
Context.Finish;
```

## Lifetime of Passed Objects

All objects that are passed to the rendering context (such as gradients, patterns, images, and paths) must live through asynchronous processing. To simplify this Blend2D automatically manages the lifetime of such objects by using reference counting. When an object is passed to the rendering context, and the context decides to keep that object for later use, it would temporarily increase the reference count of such object. In other words, it creates a weak copy of it so that it can be used by a worker thread later. Blend2D uses atomic reference counting, which means that styles can be shared between threads without races.

There is one important detail regarding reference counting in Blend2D, called copy-on-write. When two `TBLPath` instances share the same data and one instance is being changed, the data has to be copied first so that the other instance's data doesn't change as well. It is called copy-on-write, because the copy will only be made if such object is changed.

Let's demonstrate how it works:

```Delphi
var CreateInfo: TBLContextCreateInfo;
CreateInfo.Reset;
CreateInfo.ThreadCount := 2;

var Image := TBLImage.Create(500, 500, TBLFormat.Prgb32);
var Context := TBLContext.Create(Image, CreateInfo);

Context.FillAll(TAlphaColors.Black);

begin
  { Let's construct a path - now it will have a reference count 1. }
  var Path: TBLPath;
  Path.AddRoundRect(BLRoundRect(25, 25, 450, 450, 25));

  { The reference count of path may change depending on the rendering type
    and path complexity:

      - Synchronous rendering - path is consumed directly so there is no
        need to keep it. The rendering context uses the path to build edges
        and then such edges will be used by the rasterizer.

      - Asynchronous rendering - depending on path complexity the rendering
        context may construct edges for the rasterizer directly or it may
        decide to keep the path and construct edges later. If it decides to
        keep the path then its reference count will be increased by 1. }
  Context.FillPath(Path, TAlphaColors.White);
  
  { Now the path destructor will be called (since Path went out of scope). 
    If the reference count is greater than one then the path data will 
    outlive this scope and the rendering context will destroy it after 
    it's been  processed. }  
end;

{ Even if the path data was still alive at this point, 
  it won't outlive neither Flush nor Finish calls. }
Context.Finish;
```

The same scenario applies to images. E.g. the in asynchronous case it's not possible to blit an image, and then immediately change its content and then blit it again. To be more precise it would be possible, but when changing the image a deep copy of the image will be made before attempting to change it.

```Delphi
var CreateInfo: TBLContextCreateInfo;
CreateInfo.Reset;
CreateInfo.ThreadCount := 2;

var Image := TBLImage.Create(500, 500, TBLFormat.Prgb32);
var Context := TBLContext.Create(Image, CreateInfo);

var Sprite: TBLImage;
Sprite.ReadFromFile('SomeSprite.png');

Context.FillAll(TAlphaColors.Black);

{ If the rendering context is asynchronous the sprite's reference count will
  be increased by one by the rendering context as it won't blit it now. }
Context.BlitImage(BLPointI(0, 0), Sprite);

{ Now the sprite's reference count may be greater than zero, which means that
  a copy will have to be made if it has to be changed. For example attaching
  a different rendering context to that image would make a copy of it. }
begin
  var Context2 := TBLContext.Create(Sprite);
  Context2.FillRect(BLRectI(10, 10, 100, 100), TAlphaColors.White);
end;

{ If the rendering context is asynchronous the sprite's reference count will
  be increased by one again, but in this case the sprite's internal data will
  already be different compared to the first blitted sprite. This means that
  after this call the rendering context will have weak copies of two independent
  images.}
Context.BlitImage(BLPointI(200,20 0), Sprite);

{ As usual - Finish will drop all references to images passed to BlitImage. }
Context.Finish;
```

In the example above the user would actually get the same result in both the synchronous and asynchronous case. However, in the asynchronous case the image data will be copied by the secondary rendering context as it cannot start rendering into a shared image that is still in use by the primary rendering context.

## Geometries Are Lightweight

The rendering context can render paths and simpler geometries that are passed as Delphi records (like `TBLCircle`). Since the multi-threaded rendering context has to retain all paths passed to it, it's sometimes cheaper to use geometries instead. For example to fill a rounded rectangle, use `FillRoundRect` instead of constructing a path and then filling it via `FillPath`. To retain a geometry the rendering context just copies its data to a buffer, which would be then used by worker threads. Making a copy of a small Delphi record is much cheaper than managing a shared resource.

For example, consider the following example:

```Delphi
var CreateInfo: TBLContextCreateInfo;
CreateInfo.Reset;
CreateInfo.ThreadCount := 2;

var Image := TBLImage.Create(500, 500, TBLFormat.Prgb32);
var Context := TBLContext.Create(Image, CreateInfo);

Context.FillAll(TAlphaColors.Black);

{ This rounded rectangle will be serialized as is, it won't use TBLPath. }
Context.FillRoundRect(BLRoundRect(100, 100, 400, 100, 50), TAlphaColors.Red));

{ This rounded rectangle is in fact a TBLPath, and the rendering context
  will have to retain it. }
var P: TBLPath;
P.AddRoundRect(BLRoundRect(100, 300, 400, 100, 50));
Context.FillPath(P, TAlphaColors.Blue);

Context.Finish;
```

## Conclusion

This section has illustrated how the multi-threaded rendering context changes the lifetime of passed objects. It should have also been clarified that the user may expect those objects released by either calling `Flush` with particular flags or `Finish`. Finally, it has described the functionality of copy-on-write which results in deep copy of objects that are mutated and still in use by the rendering context.

It's also worth noting that calling `Flush` with `TBLContextFlushFlag.Sync` would start worker threads and wait for all of them to complete. These calls should be considered exceptional and not be overused. It isn't cheap to wake up all worker threads and wait for them to complete their work, which is the main reason that the rendering context uses batching. The result is a minimal synchronization and maximum throughput.
