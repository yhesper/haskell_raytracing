# haskell_raytracing

## Project Idea: Implementing A Monte-Carlo Ray Tracer in Haskell

We proposed to implement a physically-based Monte-Carlo Ray Tracer within the terminal using Haskell. Ray tracing is a very classic computer graphics algorithm, it simulates the light ray scattering and bouncing within the scene and records respective radiance onto an image. Ray tracing is naturally recursive, requires a set of abstract data types (ADT) like trees and maps and also needs to maintain rendering states, which makes this application a perfect testbed for functional programming. 

The application takes in a scene description containing a camera configuration, lighting, and geometry. Typically, there are multiple different types of lights and geometric shapes, we will define type classes to handle different scene configurations in a uniform way. To simulate one light path scattering in the scene, we will trace a light ray from the light source, which reflects and refracts in the scene until it reaches the camera. Each reflection or refraction could lead to a new tracing event, which can be naturally implemented in recursive functions. To display the rendered image, we need to store the result of each light path in an image, which is the state that will be implemented using monads. Finally, we will show the results in a beautiful colored terminal UI with the help of the brick library.

## Team Members

- Hesper Yin
- Wesley Chang
- Xuanda Yang
