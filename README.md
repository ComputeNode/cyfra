# Cyfra

Library provides a way to compile Scala 3 DSL to SPIR-V and to run it with Vulkan runtime on GPUs.

It is multiplatform. It works on:
 - Linux, Windows, and Mac (for Mac requires installation of moltenvk).
 - Any dedicated or integrated GPUs that support Vulkan. In practice, it means almost all moderately modern devices from most manufacturers including Nvidia, AMD, Intel, Apple.

Library is in an early stage - alpha release and proper documentation are coming.

## Animations

Included Foton library provides a clean and fun way to animate functions and ray traced scenes.

## Examples

### Ray traced animation
![output](https://github.com/user-attachments/assets/3eac9f7f-72df-4a5d-b768-9117d651c78d)

[code](https://github.com/ComputeNode/cyfra/blob/master/src/main/scala/io/computenode/cyfra/samples/foton/AnimatedRaytrace.scala)
(this is API usage, to see ray tracing implementation look at [RtRenderer](https://github.com/ComputeNode/cyfra/blob/main/src/main/scala/io/computenode/cyfra/foton/rt/RtRenderer.scala))

### Animated Julia set
<img src="assets/julia.gif" width="360">

[code](https://github.com/ComputeNode/cyfra/blob/master/src/main/scala/io/computenode/cyfra/samples/foton/AnimatedJulia.scala)

## Animation features examples

### Custom animated functions
<img src="https://github.com/user-attachments/assets/1030d968-014a-4c2c-8f21-26b999fe57fc" width="650">

### Animated ray traced scene
<img src="https://github.com/user-attachments/assets/a4189bc3-e2a9-4e52-9363-93f83b530595" width="750">

## Coding features examples

### Case classes as GPU structs

<img src="https://github.com/user-attachments/assets/e5e2020d-fcd3-4651-b0e2-fc45567df37b" width="550">

### GSeq

<img src="https://github.com/scalag/scalag/assets/4761866/bc91caf3-d9bd-4eb7-940b-be278d928243" width="600">


<img src="https://github.com/scalag/scalag/assets/4761866/2791afd8-0b3e-4113-8e01-3f4efccab37f" width="750">

### Reduce GPU resulltion 
To reduce the GPU resolution during rendering, you can adjust the parameters passed to the ``AnimatedFunctionRenderer``

### Render the frames
 1. Install `ffmpeg` if you haven't already.
 2. Render to MP4:

Render to MP4 :

  ``ffmpeg -framerate 30 -i frame%02d.png -vf "scale=540:540" -c:v libx264 -pix_fmt yuv420p output_video.mp4``

 3. Render to GIF:

Renderto to Gif

``ffmpeg -t 3 -i output_video.mp4 -vf "fps=30,scale=720:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 output_git.gif``

