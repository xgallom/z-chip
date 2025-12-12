#include <zengine.hlsl>

cbuffer FragUniformBuffer : register(b0, space3) {
    float2 scr_scl;
}

Texture2D<float4> SrcBuffer : register(t0, space2);
SamplerState SrcSampler  : register(s0, space2);

float4 main(float2 screen_pos : TEXCOORD) : SV_Target {
    float2 uv = screen_pos;
    uv = uv * scr_scl;
    uv = uv * 0.5f + 0.5f; 
    const float2 tl = step(0, uv);
    const float2 br = step(uv, 1);
    const float mask = tl.x * tl.y * br.x * br.y;
    const float4 emul_px = SrcBuffer.Sample(SrcSampler, uv);
    return float4(emul_px.rgb, mask);
}
