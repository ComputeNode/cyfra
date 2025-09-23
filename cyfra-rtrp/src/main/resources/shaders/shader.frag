#version 450

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragUV;

layout(location = 0) out vec4 outColor;

layout(binding = 0) readonly buffer DataBuffer {
    vec3 colors[];
} dataBuffer;

layout(push_constant) uniform PushConstants {
    int width;
} pushConstants;

void main() {
    int x = int(fragUV.x * pushConstants.width);
    int y = int(fragUV.y * pushConstants.width);
    int index = y * pushConstants.width + x;

    outColor = vec4(dataBuffer.colors[index], 1.0);
}