# Neslib.Sdl3.IO

!!! abstract "Reference"

    [Neslib.Sdl3.IO](../Reference/Neslib.Sdl3.IO/index.md)

## Storage Abstraction

The storage API is a high-level API designed to abstract away the portability issues that come up when using something lower-level (in SDL's case, this sits on top of the filesystem and IOStream subsystems). It is significantly more restrictive than a typical filesystem API, for a number of reasons:

1. **What to Access:** A common pitfall with existing filesystem APIs is the assumption that all storage is monolithic. However, many other platforms (game consoles in particular) are more strict about what *type* of filesystem is being accessed; for example, game content and user data are usually two separate storage devices with entirely different characteristics (and possibly different low-level APIs altogether!).
2. **How to Access:** Another common mistake is applications assuming that all storage is universally writeable - again, many platforms treat game content and user data as two separate storage devices, and only user data is writeable while game content is read-only.
3. **When to Access:** The most common portability issue with filesystem access is *timing* - you cannot always assume that the storage device is always accessible all of the time, nor can you assume that there are no limits to how long you have access to a particular device.

Consider the following example:

```delphi
procedure ReadGameData(const AFilenames: TArray<String>);
begin
  for var Filename in AFilenames do
  begin
    try
      var Stream := TFileStream.Create(Filename,
        fmOpenRead or fmShareDenyWrite);
      try
        // A bunch of stuff happens here
      finally
        Stream.Free;
      end;
    except
      // Something bad happened
    end;
  end;
end;

procedure ReadSave;
begin
  try
    var Save := TFileStream.Create('saves/save0.sav',
      fmOpenRead or fmShareDenyWrite);
    try
      // A bunch of stuff happens here
    finally
      Stream.Free;
    end;
  except
    // Something bad happened
  end;
end;

procedure WriteSave;
begin
  try
    var Save := TFileStream.Create('saves/save0.sav', 
      fmCreate or fmShareDenyWrite);
    try
      // A bunch of stuff happens here
    finally
      Stream.Free;
    end;
  except
    // Something bad happened
  end;
end;
```

Going over the bullet points again:

1. **What to Access:** This code accesses a global filesystem; game data and saves are all presumed to be in the current working directory (which may or may not be the game's installation folder!).
2. **How to Access:** This code assumes that content paths are writeable, and that save data is also writeable despite being in the same location as the game data.
3. **When to Access:** This code assumes that they can be called at any time, since the filesystem is always accessible and has no limits on how long the filesystem is being accessed.

Due to these assumptions, the filesystem code is not portable and will fail under these common scenarios:

- The game is installed on a device that is read-only, both content loading and game saves will fail or crash outright
- Game/User storage is not implicitly mounted, so no files will be found for either scenario when a platform requires explicitly mounting filesystems
- Save data may not be safe since the I/O is not being flushed or validated, so an error occurring elsewhere in the program may result in missing/corrupted save data

When using [TSdlStorage](../Reference/Neslib.Sdl3.IO/classes/TSdlStorage.md), these types of problems are virtually impossible to trip over:

```delphi
procedure ReadGameData(const AFilenames: TArray<String>);
begin
  var Title := TSdlStorage.OpenTitle;
  try
    while (not Title.IsReady) do
      SdlDelay(1);

    for var Filename in AFilenames do
    begin
      var Data := Title.ReadFile(Filename);
      // A bunch of stuff happens here
    end;
  finally
    Title.Free;
  end;
end;

procedure ReadSave;
begin
  var User := TSdlStorage.OpenUser('libsdl', 'Storage Example');
  try
    while (not User.IsReady) do
      SdlDelay(1);

    var Data := User.ReadFile('save0.sav');
    // A bunch of stuff happens here
  finally
    User.Free;
  end;
end;

procedure WriteSave(const AData: TBytes);
begin
  var User := TSdlStorage.OpenUser('libsdl', 'Storage Example');
  try
    while (not User.IsReady) do
      SdlDelay(1);

    // A bunch of stuff happens here
    User.WriteFile('save0.sav', AData);
  finally
    User.Free;
  end;
end;
```

Note the improvements that [TSdlStorage](../Reference/Neslib.Sdl3.IO/classes/TSdlStorage.md) makes:

1. **What to Access:** This code explicitly reads from a title or user storage device based on the context of the function.
2. **How to Access:** This code explicitly uses either a read or write function based on the context of the function.
3. **When to Access:** This code explicitly opens the device when it needs to, and closes it when it is finished working with the filesystem.

The result is an application that is significantly more robust against the increasing demands of platforms and their filesystems!

A publicly available example of an [TSdlStorage](../Reference/Neslib.Sdl3.IO/classes/TSdlStorage.md) backend is the [Steam Cloud](https://partner.steamgames.com/doc/features/cloud) backend - you can initialize Steamworks when starting the program, and then SDL will recognize that Steamworks is initialized and automatically use ISteamRemoteStorage when the application opens user storage. More importantly, when you *open* storage it knows to begin a "batch" of filesystem operations, and when you *close* storage it knows to end and flush the batch. This is used by Steam to support [Dynamic Cloud Sync](https://steamcommunity.com/groups/steamworks/announcements/detail/3142949576401813670) ; users can save data on one PC, put the device to sleep, and then continue playing on another PC (and vice versa) with the save data fully synchronized across all devices, allowing for a seamless experience without having to do full restarts of the program.

### Notes on valid paths

All paths in the Storage API use Unix-style path separators ('/'). Using a different path separator will not work, even if the underlying platform would otherwise accept it. This is to keep code using the Storage API portable between platforms and Storage implementations and simplify app code.

Paths with relative directories ("." and "..") are forbidden by the Storage API.

All valid Unicode strings (discounting the '/' path separator) are usable for filenames, however, an underlying Storage implementation may not support particularly strange sequences and refuse to create files with those names, etc.

## I/O Streams

SDL provides an abstract interface for reading and writing data streams. It offers implementations for files, memory, etc, and the app can provide their own implementations, too.

[TSdlIOStream](../Reference/Neslib.Sdl3.IO/classes/TSdlIOStream.md) is not related to the standard Delphi stream classes, other than both are abstract interfaces to read/write data.

## Async I/O

SDL offers a way to perform I/O asynchronously. This allows an app to read or write files without waiting for data to actually transfer; the functions that request I/O never block while the request is fulfilled.

Instead, the data moves in the background and the app can check for results at their leisure.

This is more complicated than just reading and writing files in a synchronous way, but it can allow for more efficiency, and never having framerate drops as the hard drive catches up, etc.

The general usage pattern for async I/O is:

- Create one or more [TSdlAsyncIOQueue](../Reference/Neslib.Sdl3.IO/classes/TSdlAsyncIOQueue.md) objects.
- Open files with [TSdlAsyncIO.Create](../Reference/Neslib.Sdl3.IO/classes/TSdlAsyncIO.md/#Create).
- Start I/O tasks to the files with [TSdlAsyncIO.Read](../Reference/Neslib.Sdl3.IO/classes/TSdlAsyncIO.md/#Read) or [TSdlAsyncIO.Write](../Reference/Neslib.Sdl3.IO/classes/TSdlAsyncIO.md/#Write), putting those tasks into one of the queues.
- Later on, use [TSdlAsyncIOQueue.GetResult](../Reference/Neslib.Sdl3.IO/classes/TSdlAsyncIOQueue.md/#GetResult) on a queue to see if any task is finished without blocking. Tasks might finish in any order with success or failure.
- When all your tasks are done, close the file with [TSdlAsyncIO.Free](../Reference/Neslib.Sdl3.IO/classes/TSdlAsyncIO.md/#Free). This also generates a task, since it might flush data to disk!

This all works, without blocking, in a single thread, but one can also wait on a queue in a background thread, sleeping until new results have arrived:

- Call [TSdlAsyncIOQueue.WaitResult](../Reference/Neslib.Sdl3.IO/classes/TSdlAsyncIOQueue.md/#WaitResult) from one or more threads to efficiently block until new tasks complete.
- When shutting down, call [TSdlAsyncIOQueue.Signal](../Reference/Neslib.Sdl3.IO/classes/TSdlAsyncIOQueue.md/#Signal) to unblock any sleeping threads despite there being no new tasks completed.

And, of course, to match the synchronous [SdlLoad](../Reference/Neslib.Sdl3.IO/routines/SdlLoad_0.md), we offer [SdlLoadAsync](../Reference/Neslib.Sdl3.IO/routines/SdlLoadAsync.md) as a convenience routine. This will handle allocating a buffer, slurping in the file data, and null-terminating it; you still check for results later.

Behind the scenes, SDL will use newer, efficient APIs on platforms that support them: Linux's io_uring and Windows 11's IoRing, for example. If those technologies aren't available, SDL will offload the work to a thread pool that will manage otherwise-synchronous loads without blocking the app.

### Best Practices

Simple non-blocking I/O--for an app that just wants to pick up data whenever it's ready without losing framerate waiting on disks to spin--can use whatever pattern works well for the program. In this case, simply call [TSdlAsyncIO.Read](../Reference/Neslib.Sdl3.IO/classes/TSdlAsyncIO.md/#Read), or maybe [SdlLoadAsync](../Reference/Neslib.Sdl3.IO/routines/SdlLoadAsync.md), as needed. Once a frame, call [TSdlAsyncIOQueue.GetResult](../Reference/Neslib.Sdl3.IO/classes/TSdlAsyncIOQueue.md/#GetResult) to check for any completed tasks and deal with the data as it arrives.

If two separate pieces of the same program need their own I/O, it is legal for each to create their own queue. This will prevent either piece from accidentally consuming the other's completed tasks. Each queue does require some amount of resources, but it is not an overwhelming cost. Do not make a queue for each task, however. It is better to put many tasks into a single queue. They will be reported in order of completion, not in the order they were submitted, so it doesn't generally matter what order tasks are started.

One async I/O queue can be shared by multiple threads, or one thread can have more than one queue, but the most efficient way--if ruthless efficiency is the goal--is to have one queue per thread, with multiple threads working in parallel, and attempt to keep each queue loaded with tasks that are both started by and consumed by the same thread. On modern platforms that can use newer interfaces, this can keep data flowing as efficiently as possible all the way from storage hardware to the app, with no contention between threads for access to the same queue.

Written data is not guaranteed to make it to physical media by the time a closing task is completed, unless [TSdlAsyncIO.Free](../Reference/Neslib.Sdl3.IO/classes/TSdlAsyncIO.md/#Free) is called with its `AFlush` parameter set to True, which is to say that a successful result here can still result in lost data during an unfortunately-timed power outage if not flushed. However, flushing will take longer and may be unnecessary, depending on the app's needs.
