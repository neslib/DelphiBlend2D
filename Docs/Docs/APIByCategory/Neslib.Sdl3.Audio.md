# Neslib.Sdl3.Audio

!!! abstract "Reference"

    [Neslib.Sdl3.Audio](../Reference/Neslib.Sdl3.Audio/index.md)

All audio in SDL3 revolves around [TSdlAudioStream](../Reference/Neslib.Sdl3.Audio/classes/TSdlAudioStream.md). Whether you want to play or record audio, convert it, stream it, buffer it, or mix it, you're going to be passing it through an audio stream.

Audio streams are quite flexible; they can accept any amount of data at a time, in any supported format, and output it as needed in any other format, even if the data format changes on either side halfway through.

An app opens an audio device and binds any number of audio streams to it, feeding more data to the streams as available. When the device needs more data, it will pull it from all bound streams and mix them together for playback.

Audio streams can also use an app-provided callback to supply data on-demand, which maps pretty closely to the SDL2 audio model.

SDL also provides a simple .WAV loader with [TSdlAudioBuffer.CreateFromWav](../Reference/Neslib.Sdl3.Audio/classes/TSdlAudioBuffer.md/#CreateFromWav_0) as a basic means to load sound data into your program. This constructor can load a WAV file from a file or from a [TSdlIOStream](../Reference/Neslib.Sdl3.IO/classes/TSdlIOStream.md).

## Logical audio devices

In SDL3, opening a physical device (like a SoundBlaster 16 Pro) gives you a logical device ID that you can bind audio streams to. In almost all cases, logical devices can be used anywhere in the API that a physical device is normally used. However, since each device opening generates a new logical device, different parts of the program (say, a VoIP library, or text-to-speech framework, or maybe some other sort of mixer on top of SDL) can have their own device opens that do not interfere with each other; each logical device will mix its separate audio down to a single buffer, fed to the physical device, behind the scenes. As many logical devices as you like can come and go; SDL will only have to open the physical device at the OS level once, and will manage all the logical devices on top of it internally.

One other benefit of logical devices: if you don't open a specific physical device, instead opting for the default, SDL can automatically migrate those logical devices to different hardware as circumstances change: a user plugged in headphones? The system default changed? SDL can transparently migrate the logical devices to the correct physical device seamlessly and keep playing; the app doesn't even have to know it happened if it doesn't want to.

## Simplified audio

As a simplified model for when a single source of audio is all that's needed, an app can use a version of [TSdlAudioStream.Create](../Reference/Neslib.Sdl3.Audio/classes/TSdlAudioStream.md/#Create_0) that opens an audio device, creates an audio stream, binds that stream to the newly-opened device, and (optionally) provides a callback for obtaining audio data. When using this constructor, the primary interface is the [TSdlAudioStream](../Reference/Neslib.Sdl3.Audio/classes/TSdlAudioStream.md) and the device handle is mostly hidden away; destroying a stream created through this constructor will also close the device, stream bindings cannot be changed, etc. One other quirk of this is that the device is started in a *paused* state and must be explicitly resumed; this is partially to offer a clean migration for SDL2 apps and partially because the app might have to do more setup before playback begins; in the non-simplified form, nothing will play until a stream is bound to a device, so they start *unpaused*.

## Channel layouts

Audio data passing through SDL is uncompressed PCM data, interleaved. One can provide their own decompression through an MP3, etc, decoder, but SDL does not provide this directly. Each interleaved channel of data is meant to be in a specific order.

Abbreviations:

- FRONT = single mono speaker
- FL = front left speaker
- FR = front right speaker
- FC = front center speaker
- BL = back left speaker
- BR = back right speaker
- SR = surround right speaker
- SL = surround left speaker
- BC = back center speaker
- LFE = low-frequency speaker

These are listed in the order they are laid out in memory, so "FL, FR" means "the front left speaker is laid out in memory first, then the front right, then it repeats for the next audio frame".

- 1 channel (mono) layout: FRONT
- 2 channels (stereo) layout: FL, FR
- 3 channels (2.1) layout: FL, FR, LFE
- 4 channels (quad) layout: FL, FR, BL, BR
- 5 channels (4.1) layout: FL, FR, LFE, BL, BR
- 6 channels (5.1) layout: FL, FR, FC, LFE, BL, BR (last two can also be SL, SR)
- 7 channels (6.1) layout: FL, FR, FC, LFE, BC, SL, SR
- 8 channels (7.1) layout: FL, FR, FC, LFE, BL, BR, SL, SR

This is the same order as DirectSound expects, but applied to all platforms; SDL will swizzle the channels as necessary if a platform expects something different.

[TSdlAudioStream](../Reference/Neslib.Sdl3.Audio/classes/TSdlAudioStream.md) can also be provided channel maps to change this ordering to whatever is necessary, in other audio processing scenarios.
