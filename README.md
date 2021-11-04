### DXBC Reader
A dxbc reader with rust implementation. It is not well done now and need more time to finish it.

### Build
* Rust 1.58.0-nightly

### Run
dxbc_reader [IN={Pos tex0 tex1 color}] [OUT={SV_Position tex color}] input.dxbc

### Todo
* Semantic Analysis for the labels for different usage(variable).
* ConstantBuffer defines

### Examples

    mul r0.xyzw, v0.yyyy, cb1[1].xyzw
    mad r0.xyzw, cb1[0].xyzw, v0.xxxx, r0.xyzw
    mad r0.xyzw, cb1[2].xyzw, v0.zzzz, r0.xyzw
    mad r0.xyzw, cb1[3].xyzw, v0.wwww, r0.xyzw
    mad r0.xyzw, v2.xxxx, cb0[4].xyzw, r0.xyzw
    mad r0.xyzw, v2.yyyy, cb0[5].xyzw, r0.xyzw
    mul r1.xyz, r0.yyyy, cb2[18].xywx
    mad r1.xyz, cb2[17].xywx, r0.xxxx, r1.xyzx
    mad r0.xyz, cb2[19].xywx, r0.zzzz, r1.xyzx
    mad o0.xyw, cb2[20].xyxw, r0.wwww, r0.xyxz
    mov o0.z, l(0)
    mov o1.xy, v1.xyxx
    add r0.x, -v3.w, cb0[2].y
    add r0.y, -cb0[2].w, l(1.000000)
    mad r0.x, |r0.x|, r0.y, cb0[2].w
    mul_sat r0.xyzw, r0.xxxx, v3.xyzw
    mul o2.xyzw, r0.xyzw, cb0[2].zzzz

Output：

    local_10.xyzw =  _IN.Pos.y * _CB[1].xyzw
    local_10.xyzw =  _CB[0].xyzw * _IN.Pos.x + local_10.xyzw
    local_10.xyzw =  _CB[2].xyzw * _IN.Pos.z + local_10.xyzw
    local_10.xyzw =  _CB[3].xyzw * _IN.Pos.w + local_10.xyzw
    local_10.xyzw =  _IN.tex1.x * _CB[4].xyzw + local_10.xyzw
    local_10.xyzw =  _IN.tex1.y * _CB[5].xyzw + local_10.xyzw
    local_13.xyz =  local_10.y * _CB[18].xyw
    local_13.xyz =  _CB[17].xyw * local_10.x + local_13.xyz
    local_10.xyz =  _CB[19].xyw * local_10.z + local_13.xyz
    _OUT.SV_Position.xyw =  _CB[20].xyw * local_10.w + local_10.xyz
    _OUT.SV_Position.z =  float(0)
    _OUT.tex.xy =  _IN.tex0.xy
    local_10.x =  local_15.w + _CB[2].y
    local_10.y =  local_16[2].w + float(1)
    local_10.x =  local_10.x * local_10.y + _CB[2].w
    local_10.xyzw =  saturate( local_10.x * _IN.color.xyzw )
    _OUT.color.xyzw =  local_10.xyzw * _CB[2].z

