package io.computenode.cyfra.rtrp

    val context = new vulkanContext() 
    // shader modules
    val vertShaderCode = Shader.loadShader("shaders/vert.spv");
    val fragShaderCode = Shader.loadShader("shaders/frag.spv");

    val vLayoutInfo = LayoutInfo
    val fLayoutInfo = LayoutInfo

    val vertShader = new Shader(vertShaderCode, vLayoutInfo, "main", device)
    val fragShader = new Shader(fragShaderCode, fLayoutInfo, "main", device)

    val pipeline(vertShader, fragShader, context)