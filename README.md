### DXBC Reader
A dxbc reader with rust implementation. It is not well done now and need more time to finish it.

### Build
* Rust 1.58.0-nightly

### Run
dxbc_reader input.dxbc

### Todo
*   Semtanic Analysis for the labels for different usage(variable).

### Examples

    sample r0.xyzw, v4.xyxx, t0.xyzw, s1
    mad r1.xyz, r0.xyzx, l(0.305306, 0.305306, 0.305306, 0.000000), l(0.682171, 0.682171, 0.682171, 0.000000)
    mad r1.xyz, r0.xyzx, r1.xyzx, l(0.012523, 0.012523, 0.012523, 0.000000)
    mul r0.xyz, r0.xyzx, r1.xyzx
    mul r1.xyz, cb0[75].wwww, cb0[75].xyzx
    mul r1.xyz, r1.xyzx, l(10.000000, 10.000000, 10.000000, 0.000000)
    mad r0.xyz, r0.xyzx, r1.xyzx, l(0.018000, 0.018000, 0.018000, 0.000000)
    dp3 r0.w, v3.xyzx, v3.xyzx
    max r0.w, r0.w, l(0.001000)
    rsq r0.w, r0.w
    mul r1.xyz, r0.wwww, v3.xyzx
    dp3 r0.w, r1.xyzx, cb2[0].xyzx
    mad r1.xy, r1.yyyy, l(0.320000, -0.017500, 0.000000, 0.000000), l(0.480000, 0.032500, 0.000000, 0.000000)
    max r0.w, r0.w, l(0.000000)
    mul r2.xyz, r0.wwww, cb0[18].xyzx
    mul r2.xyz, r0.xyzx, r2.xyzx
    mul r1.xzw, r1.xxxx, cb0[27].xxyz
    mad r1.xyz, cb0[16].xyzx, r1.yyyy, r1.xzwx
    mad r0.xyz, r0.xyzx, r1.xyzx, r2.xyzx
    mul r0.xyz, r0.xyzx, v2.wwww
    mad r0.w, -v2.z, v2.w, v2.z
    mad r0.xyz, cb0[26].xyzx, r0.wwww, r0.xyzx
    mul r1.xy, v0.xyxx, cb1[6].zwzz
    sample_l r1.xyzw, r1.xyxx, t1.xyzw, s0, l(0.000000)
    mad r2.xyz, r1.wwww, cb0[23].xyzx, r1.xyzx
    add r1.xyz, r1.xyzx, -r2.xyzx
    mad r1.xyz, v2.wwww, r1.xyzx, r2.xyzx
    add r0.xyz, r0.xyzx, -r1.xyzx
    mad r0.xyz, v2.xyzx, r0.xyzx, r1.xyzx
    mov o0.xyz, r0.xyzx
    mov_sat r0.xyz, r0.xyzx
    sqrt o2.xyz, r0.xyzx
    mov o0.w, l(1.000000)
    mov o1.xyz, v0.zzzz
    mov o1.w, l(1.000000)
    mov o2.w, l(1.000000)

Output：

    local_10.xyz =  local_11.xyz * float3(0.3053,0.3053,0.3053) + float3(0.6821,0.6821,0.6821)
    local_10.xyz =  local_11.xyz * local_10.xyz + float3(0.0125,0.0125,0.0125)
    local_11.xyz =  local_11.xyz * local_10.xyz
    local_10.xyz =  _CB[75].w * _CB[75].xyz
    local_10.xyz =  local_10.xyz * float3(10,10,10)
    local_11.xyz =  local_11.xyz * local_10.xyz + float3(0.018,0.018,0.018)
    local_11.w =  dot( _IN.3.xyz, _IN.3.xyz )
    local_11.w =  1.0  / sqrt( local_11.w )
    local_10.xyz =  local_11.w * _IN.3.xyz
    local_11.w =  dot( local_10.xyz, _CB[0].xyz )
    local_10.xy =  local_10.y * float2(0.32,-0.017) + float2(0.48,0.0325)
    local_15.xyz =  local_11.w * _CB[18].xyz
    local_15.xyz =  local_11.xyz * local_15.xyz
    local_10.xzw =  local_10.x * _CB[27].xyz
    local_10.xyz =  _CB[16].xyz * local_10.y + local_10.xzw
    local_11.xyz =  local_11.xyz * local_10.xyz + local_15.xyz
    local_11.xyz =  local_11.xyz * _IN.2.w
    local_11.w =  local_17.z * _IN.2.w + _IN.2.z
    local_11.xyz =  _CB[26].xyz * local_11.w + local_11.xyz
    local_10.xy =  _IN.0.xy * _CB[6].zw
    local_15.xyz =  local_10.w * _CB[23].xyz + local_10.xyz
    local_10.xyz =  local_10.xyz + local_20.xyz
    local_10.xyz =  _IN.2.w * local_10.xyz + local_15.xyz
    local_11.xyz =  local_11.xyz + local_21.xyz
    local_11.xyz =  _IN.2.xyz * local_11.xyz + local_10.xyz
    _OUT.0.xyz =  local_11.xyz
    local_11.xyz =  saturate( local_11.xyz )
    _OUT.2.xyz =  sqrt(local_11.xyz)
    _OUT.0.w =  float(1)
    _OUT.1.xyz =  _IN.0.z
    _OUT.1.w =  float(1)
    _OUT.2.w =  float(1)
