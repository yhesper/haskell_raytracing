# haskell_raytracing

## Project Idea: Implementing a Bidirectional Path Tracing Renderer in Haskell

We proposed to implement a physically-based renderer within the terminal using Haskell. Ray tracing is a very classic computer graphics algorithm that renders a 3D scene to an image by simulating light rays scattering and bouncing within the scene, recording the light carried by the rays at pixels in the image. Ray tracing is naturally recursive, requires a set of abstract data types (ADT) like trees and maps and also needs to maintain rendering states, which makes this application a perfect testbed for functional programming. We will specifically implement the bidirectional path tracing algorithm, which is an advanced form of ray tracing that forms complete light paths from the camera to light sources by tracing rays *bidirectionally*, from both the camera and the light sources, as opposed to *unidirectionally* from only the camera, as in standard path tracers.

The application takes in a scene description containing a camera configuration, lighting, and geometry. Typically, there are multiple different types of lights and geometric shapes, we will define type classes to handle different scene configurations in a uniform way. To simulate one light path scattering in the scene, we will trace a light ray from the light source, which reflects and refracts in the scene. We will then similarly trace a light ray from the camera, and connect the two subpaths. Each reflection or refraction could lead to a new tracing event, which can be naturally implemented in recursive functions. To display the rendered image, we need to store the result of each light path in an image, which is the state that will be implemented using monads. Finally, we will show the results in a beautiful colored terminal UI with the help of the brick library.

## Team Members

- Hesper Yin
- Wesley Chang
- Xuanda Yang


## Milestone 2: Updates

### Architecture

our project includes two major key components: A user interface powered by the brick library to display the progression of our rendering algorithm, with a progress bar indicating how much work is to be done and sliders for users to adjust the rendered scene properties (For example users can interactively adjust the material of the object and see the rendering results reflect the changes). The user interface takes image buffers rendered by our bidirectional rendering algorithm and then rasterizes a fixed 25x25 image. The other major component is the bidirectional path tracer, which read our pre-defined scene description, constructs light paths, traces light paths, and finally records the result. The two components have a clear producer-consumer relationship where the path tracer produces the image and the UI consumes it.

### Challenges so far:
- Designing the UI is challenging. We need to understand brick library's common patterns of building an app, so we spent time reading and learning from brick's official code samples and refined our code to render a demo scene in the command line.
- We need to design a scene that 1. be able to be rendered by our app. 2. have enough customization options so users can play around with it. We started with the classic cornell box scene (https://www.wikiwand.com/en/Cornell_box), perturbed the geometry and made one of the box metal-ish materials, which allows user to explore both diffuse and specular visual effects.
- Designing proper ADT for our bidirectional rendering algorithm. BDPT requires an explicit light path data structure, we need to carefully design this in a functional way.

### Do we expect to meet our goals until the deadline?
So far we expect to meet the goals until the deadline.
