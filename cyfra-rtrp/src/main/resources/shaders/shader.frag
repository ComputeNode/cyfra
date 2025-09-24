#version 450

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragUV;

layout(location = 0) out vec4 outColor;

layout(binding = 0) readonly buffer DataBuffer {
    vec4 colors[];
} dataBuffer;

layout(push_constant) uniform PushConstants {
    int width;
    int useAlpha;  // Add flag to control alpha usage
} pushConstants;

void main() {
    int x = int(fragUV.x * pushConstants.width);
    int y = int(fragUV.y * pushConstants.width);
    int index = y * pushConstants.width + x;

    vec4 computedColor = dataBuffer.colors[index];
    
    if (pushConstants.useAlpha == 1) {
        outColor = computedColor;  // Use full RGBA
    } else {
        outColor = vec4(computedColor.rgb, 1.0);  // Ignore alpha
    }
}