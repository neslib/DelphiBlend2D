# Neslib.Sdl3.Gpu

!!! abstract "Reference"

    [Neslib.Sdl3.Gpu](../Reference/Neslib.Sdl3.Gpu/index.md)

The GPU API offers a cross-platform way for apps to talk to modern graphics hardware. It offers both 3D graphics and compute support, in the style of Metal, Vulkan, and Direct3D 12.

A basic workflow might be something like this:

The app creates a [TSdlGpuDevice](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuDevice.md) and assigns it to a window with [TSdlGpuDevice.ClaimWindow](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuDevice.md/#ClaimWindow)--although strictly speaking you can render offscreen entirely, perhaps for image processing, and not use a window at all.

Next, the app prepares static data (things that are created once and used over and over). For example:

- Shaders (programs that run on the GPU): use [TSdlGpuShader](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuShader.md).
- Vertex buffers (arrays of geometry data) and other rendering data: use  [TSdlGpuBuffer](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuBuffer.md) and [TSdlGpuCopyPass.UploadToBuffer](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuCopyPass.md/#UploadToBuffer).
- Textures (images): use [TSdlGpuTexture](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuTexture.md) and [TSdlGpuCopyPass.UploadToTexture](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuCopyPass.md/#UploadToTexture).
- Samplers (how textures should be read from): use [TSdlGpuSampler](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuSampler.md).
- Render pipelines (precalculated rendering state): use [TSdlGpuGraphicsPipeline](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuGraphicsPipeline.md).

To render, the app creates one or more command buffers, with [TSdlGpuDevice.AcquireCommandBuffer](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuDevice.md/#AcquireCommandBuffer). Command buffers collect rendering instructions that will be submitted to the GPU in batch. Complex scenes can use multiple command buffers, maybe configured across multiple threads in parallel, as long as they are submitted in the correct order, but many apps will just need one command buffer per frame.

Rendering can happen to a texture (what other APIs call a "render target") or it can happen to the swapchain texture (which is just a special texture that represents a window's contents). The app can use [TSdlGpuCommandBuffer.WaitAndAcquireSwapchainTexture](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuCommandBuffer.md/#WaitAndAcquireSwapchainTexture) to render to the window.

Rendering actually happens in a Render Pass, which is encoded into a command buffer. One can encode multiple render passes (or alternate between render and compute passes) in a single command buffer, but many apps might simply need a single render pass in a single command buffer. Render Passes can render to up to four color textures and one depth texture simultaneously. If the set of textures being rendered to needs to change, the Render Pass must be ended and a new one must be begun.

The app calls [TSdlGpuCommandBuffer.BeginRenderPass](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuCommandBuffer.md/#BeginRenderPass_0). Then it sets states it needs for each draw:

- [TSdlGpuRenderPass.BindPipeline](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuRenderPass.md/#BindPipeline)
- [TSdlGpuRenderPass.SetViewport](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuRenderPass.md/#SetViewport)
- [TSdlGpuRenderPass.BindVertexBuffers](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuRenderPass.md/#BindVertexBuffers)
- [TSdlGpuRenderPass.BindVertexSamplers](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuRenderPass.md/#BindVertexSamplers)
- etc

Then, make the actual draw commands with these states:

- [TSdlGpuRenderPass.DrawPrimitives](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuRenderPass.md/#DrawPrimitives_0)
- [TSdlGpuRenderPass.DrawIndexedPrimitives](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuRenderPass.md/#DrawIndexedPrimitives_1)
- etc

After all the drawing commands for a pass are complete, the app should call [TSdlGpuRenderPass.Finish](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuRenderPass.md/#Finish). Once a render pass ends all render-related state is reset.

The app can begin new Render Passes and make new draws in the same command buffer until the entire scene is rendered.

Once all of the render commands for the scene are complete, the app calls [TSdlGpuCommandBuffer.Submit](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuCommandBuffer.md/#Submit) to send it to the GPU for processing.

If the app needs to read back data from texture or buffers, the API has an efficient way of doing this, provided that the app is willing to tolerate some latency. When the app uses [TSdlGpuCopyPass.DownloadFromTexture](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuCopyPass.md/#DownloadFromTexture) or [TSdlGpuCopyPass.DownloadFromBuffer](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuCopyPass.md/#DownloadFromBuffer), submitting the command buffer with [TSdlGpuCommandBuffer.SubmitAndAcquireFence](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuCommandBuffer.md/#SubmitAndAcquireFence) will return a fence handle that the app can poll or wait on in a thread. Once the fence indicates that the command buffer is done processing, it is safe to read the downloaded data. Make sure to call [TSdlGpuDevice.ReleaseFence](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuDevice.md/#ReleaseFence) when done with the fence.

The API also has "compute" support. The app calls [TSdlGpuCommandBuffer.BeginComputePass](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuCommandBuffer.md/#BeginComputePass) with compute-writeable textures and/or buffers, which can be written to in a compute shader. Then it sets states it needs for the compute dispatches:

- [TSdlGpuComputePass.BindPipeline](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuComputePass.md/#BindPipeline)
- [TSdlGpuComputePass.BindStorageBuffers](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuComputePass.md/#BindStorageBuffers)
- [TSdlGpuComputePass.BindStorageTextures](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuComputePass.md/#BindStorageTextures)

Then, dispatch compute work:

- [TSdlGpuComputePass.Dispatch](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuComputePass.md/#Dispatch_0)

For advanced users, this opens up powerful GPU-driven workflows.

Graphics and compute pipelines require the use of shaders, which as mentioned above are small programs executed on the GPU. Each backend (Vulkan, Metal, D3D12) requires a different shader format. When the app creates the GPU device, the app lets the device know which shader formats the app can provide. It will then select the appropriate backend depending on the available shader formats and the backends available on the platform. When creating shaders, the app must provide the correct shader format for the selected backend. If you would like to learn more about why the API works this way, there is a detailed [blog post](https://moonside.games/posts/layers-all-the-way-down/) explaining this situation.

It is optimal for apps to pre-compile the shader formats they might use, but for ease of use SDL provides a separate project, [SDL_shadercross](https://wiki.libsdl.org/SDL3/SDL_shadercross) ([Github](https://github.com/libsdl-org/SDL_shadercross)), for performing runtime shader cross-compilation. It also has a CLI interface for offline precompilation as well.

This is an extremely quick overview that leaves out several important details. Already, though, one can see that GPU programming can be quite complex! If you just need simple 2D graphics, the [Render API](Neslib.Sdl3.Video.md) is much easier to use but still hardware-accelerated. That said, even for 2D applications the performance benefits and expressiveness of the GPU API are significant.

The GPU API targets a feature set with a wide range of hardware support and ease of portability. It is designed so that the app won't have to branch itself by querying feature support. If you need cutting-edge features with limited hardware support, this API is probably not for you.

Examples demonstrating proper usage of this API can be found [here](https://github.com/TheSpydog/SDL_gpu_examples) .

## Performance considerations

Here are some basic tips for maximizing your rendering performance.

- Beginning a new render pass is relatively expensive. Use as few render passes as you can.
- Minimize the amount of state changes. For example, binding a pipeline is relatively cheap, but doing it hundreds of times when you don't need to will slow the performance significantly.
- Perform your data uploads as early as possible in the frame.
- Don't churn resources. Creating and releasing resources is expensive. It's better to create what you need up front and cache it.
- Don't use uniform buffers for large amounts of data (more than a matrix or so). Use a storage buffer instead.
- Use cycling correctly. There is a detailed explanation of cycling further below.
- Use culling techniques to minimize pixel writes. The less writing the GPU has to do the better. Culling can be a very advanced topic but even simple culling techniques can boost performance significantly.

In general try to remember the golden rule of performance: doing things is more expensive than not doing things. Don't Touch The Driver!

## FAQ

**Question: When are you adding more advanced features, like ray tracing or mesh shaders?**

Answer: We don't have immediate plans to add more bleeding-edge features, but we certainly might in the future, when these features prove worthwhile, and reasonable to implement across several platforms and underlying APIs. So while these things are not in the "never" category, they are definitely not "near future" items either.

**Question: Why is my shader not working?**

Answer: A common oversight when using shaders is not properly laying out the shader resources/registers correctly. The GPU API is very strict with how it wants resources to be laid out and it's difficult for the API to automatically validate shaders to see if they have a compatible layout. See the documentation for [TSdlGpuShader](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuShader.md) and [TSdlGpuComputePipeline](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuComputePipeline.md) for information on the expected layout.

Another common issue is not setting the correct number of samplers, textures, and buffers in [TSdlGpuShaderCreateInfo](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuShaderCreateInfo.md). If possible use shader reflection to extract the required information from the shader automatically instead of manually filling in the record's values.

**Question: My application isn't performing very well. Is this the GPU API's fault?**

Answer: No. Long answer: The GPU API is a relatively thin layer over the underlying graphics API. While it's possible that we have done something inefficiently, it's very unlikely especially if you are relatively inexperienced with GPU rendering. Please see the performance tips above and make sure you are following them. Additionally, tools like RenderDoc can be very helpful for diagnosing incorrect behavior and performance issues.

## System Requirements

**Vulkan:** Supported on Windows, Linux and certain Android devices. Requires Vulkan 1.0 with the following extensions and device features:

- `VK_KHR_swapchain`
- `VK_KHR_maintenance1`
- `independentBlend`
- `imageCubeArray`
- `depthClamp`
- `shaderClipDistance`
- `drawIndirectFirstInstance`

**D3D12:** Supported on Windows 10 or newer. Requires a GPU that supports DirectX 12 Feature Level 11_1.

**Metal:** Supported on macOS 10.14+ and iOS/tvOS 13.0+. Hardware requirements vary by operating system:

- iOS/tvOS requires an A9 GPU or newer
- iOS Simulator and tvOS Simulator are unsupported

## Uniform Data

Uniforms are for passing data to shaders. The uniform data will be constant across all executions of the shader.

There are 4 available uniform slots per shader stage (where the stages are vertex, fragment, and compute). Uniform data pushed to a slot on a stage keeps its value throughout the command buffer until you call the relevant Push function on that slot again.

For example, you could write your vertex shaders to read a camera matrix from uniform binding slot 0, push the camera matrix at the start of the command buffer, and that data will be used for every subsequent draw call.

It is valid to push uniform data during a render or compute pass.

Uniforms are best for pushing small amounts of data. If you are pushing more than a matrix or two per call you should consider using a storage buffer instead.

## A Note On Cycling

When using a command buffer, operations do not occur immediately - they occur some time after the command buffer is submitted.

When a resource is used in a pending or active command buffer, it is considered to be "bound". When a resource is no longer used in any pending or active command buffers, it is considered to be "unbound".

If data resources are bound, it is unspecified when that data will be unbound unless you acquire a fence when submitting the command buffer and wait on it. However, this doesn't mean you need to track resource usage manually.

All of the functions and records that involve writing to a resource have a `ACycle` boolean. [TSdlGpuTransferBuffer](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuTransferBuffer.md), [TSdlGpuBuffer](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuBuffer.md), and [TSdlGpuTexture](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuTexture.md) all effectively function as ring buffers on internal resources. When `ACycle` is True, if the resource is bound, the cycle rotates to the next unbound internal resource, or if none are available, a new one is created. This means you don't have to worry about complex state tracking and synchronization as long as cycling is correctly employed.

For example: you can call [TSdlGpuDevice.MapTransferBuffer](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuDevice.md/#MapTransferBuffer), write texture data, [TSdlGpuDevice.UnmapTransferBuffer](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuDevice.md/#UnmapTransferBuffer), and then [TSdlGpuCopyPass.UploadToTexture](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuCopyPass.md/#UploadToTexture). The next time you write texture data to the transfer buffer, if you set the `ACycle` param to True, you don't have to worry about overwriting any data that is not yet uploaded.

Another example: If you are using a texture in a render pass every frame, this can cause a data dependency between frames. If you set `Cycle` to True in the [TSdlGpuColorTargetInfo](../Reference/Neslib.Sdl3.Gpu/classes/TSdlGpuColorTargetInfo.md) record, you can prevent this data dependency.

Cycling will never undefine already bound data. When cycling, all data in the resource is considered to be undefined for subsequent commands until that data is written again. You must take care not to read undefined data.

Note that when cycling a texture, the entire texture will be cycled, even if only part of the texture is used in the call, so you must consider the entire texture to contain undefined data after cycling.

You must also take care not to overwrite a section of data that has been referenced in a command without cycling first. It is OK to overwrite unreferenced data in a bound resource without cycling, but overwriting a section of data that has already been referenced will produce unexpected results.
