# haskell_raytracing

## Project Idea: Implementing a Bidirectional Path Tracing Renderer in Haskell

We proposed to implement a physically-based renderer within the terminal using Haskell. Ray tracing is a very classic computer graphics algorithm that renders a 3D scene to an image by simulating light rays scattering and bouncing within the scene, recording the light carried by the rays at pixels in the image. Ray tracing is naturally recursive, requires a set of abstract data types (ADT) like trees and maps and also needs to maintain rendering states, which makes this application a perfect testbed for functional programming. We will specifically implement the bidirectional path tracing algorithm, which is an advanced form of ray tracing that forms complete light paths from the camera to light sources by tracing rays *bidirectionally*, from both the camera and the light sources, as opposed to *unidirectionally* from only the camera, as in standard path tracers.

The application takes in a scene description containing a camera configuration, lighting, and geometry. Typically, there are multiple different types of lights and geometric shapes, we will define type classes to handle different scene configurations in a uniform way. To simulate one light path scattering in the scene, we will trace a light ray from the light source, which reflects and refracts in the scene. We will then similarly trace a light ray from the camera, and connect the two subpaths. Each reflection or refraction could lead to a new tracing event, which can be naturally implemented in recursive functions. To display the rendered image, we need to store the result of each light path in an image, which is the state that will be implemented using monads. Finally, we will show the results in a beautiful colored terminal UI with the help of the brick library.

## Team Members

- Hesper Yin
- Wesley Chang
- Xuanda Yang


## Milestone 2: Updates

