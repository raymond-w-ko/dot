" Vim syntax file
" Language:	HLSL FX
" Maintainer:	Kevin Bjorke <kbjorke@nvidia.com>
" Last change:	$Date: 2003/12/05 $
" File Types:	.hlsl .fxc .fx .fxh
" $Id: //devrel/Playpen/kbjorke/doc/fx.vim#3 $

" Added: a number of useful insert-mode abbreviations for creating tweakables entries and
"	their default values. See end of the file for definitions. (":ia" will list them,
"	but cryptically)

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Read the C syntax to start with
if version < 600
  so <sfile>:p:h/c.vim
else
  runtime! syntax/c.vim
  unlet b:current_syntax
endif

" HLSL extentions
syn keyword fxStatement		tex1D tex2D tex3D texRECT texCUBE
syn keyword fxStatement		tex1Dproj tex2Dproj tex3Dproj texRECTproj texCUBEproj
" syn keyword fxStatement	tex1D_proj tex2D_proj tex3D_proj texRECT_proj texCUBE_proj
" syn keyword fxStatement	tex1D_bias tex2D_bias tex3D_bias texRECT_bias texCUBE_bias
syn keyword fxStatement		offsettex2D offsettexRECT offsettex2DScaleBias offsettexRECTScaleBias 
syn keyword fxStatement		tex1D_dp3 tex2D_dp3x2 texRECT_dp3x2
syn keyword fxStatement		tex3D_dp3x3 texCUBE_dp3x3 tex_dp3x2_depth
syn keyword fxStatement		texCUBE_reflect_dp3x3 texCUBE_reflect_eye_dp3x3
syn keyword fxStatement		discard
syn keyword fxProfile		arbfp1 arbvp1
syn keyword fxProfile		ps_1_1 ps_1_2 ps_1_3 vs_1_1 vs_2_0 vs_2_x ps_2_0 ps_2_x
syn keyword fxProfile		fp20 vp20 fp30 vp30
" many HLSL data types
syn keyword fxType		bool bool2 bool3 bool4
syn keyword fxType		bool1x2 bool1x3 bool1x4
syn keyword fxType		bool2x2 bool2x3 bool2x4
syn keyword fxType		bool3x2 bool3x3 bool3x4
syn keyword fxType		bool4x2 bool4x3 bool4x4
syn keyword fxType		half half2 half3 half4
syn keyword fxType		half1x2 half1x3 half1x4
syn keyword fxType		half2x2 half2x3 half2x4
syn keyword fxType		half3x2 half3x3 half3x4
syn keyword fxType		half4x2 half4x3 half4x4
syn keyword fxType		fixed fixed2 fixed3 fixed4
syn keyword fxType		fixed1x2 fixed1x3 fixed1x4
syn keyword fxType		fixed2x2 fixed2x3 fixed2x4
syn keyword fxType		fixed3x2 fixed3x3 fixed3x4
syn keyword fxType		fixed4x2 fixed4x3 fixed4x4
" 'float' is already a C type
syn keyword fxType		float2 float3 float4
syn keyword fxType		float1x2 float1x3 float1x4
syn keyword fxType		float2x2 float2x3 float2x4
syn keyword fxType		float3x2 float3x3 float3x4
syn keyword fxType		float4x2 float4x3 float4x4
" likewise 'int'
syn keyword fxType		int2 int3 int4
syn keyword fxType		int1x2 int1x3 int1x4
syn keyword fxType		int2x2 int2x3 int2x4
syn keyword fxType		int3x2 int3x3 int3x4
syn keyword fxType		int4x2 int4x3 int4x4
" texture types
syn keyword fxType		sampler1D sampler2D sampler3D samplerCUBE
" syn keyword fxType		samplerRECT
" compile-time special types
" syn keyword fxType		cint cfloat

syn keyword fxAnnotation	Space UIDesc UIName UIObject UIType UIMin UIMax UIStep
syn keyword fxAnnotation	Texture MinFilter MagFilter MipFilter AddressU AddressV AddressW
syn keyword fxAnnotation	usage width height levels DepthBuffer format
syn keyword fxAnnotation	geometry cleardepth clearcolor rendertarget

" how to disable switch continue case default int break goto double enum union

" syn keyword fxSamplerArg	MinFilter MagFilter MipFilter
syn match fxSamplerArg	/\<\c\(min\|mag\|mip\)filter\>/
syn keyword fxSamplerArg	AddressU AddressV AddressW

" fx
syn keyword fxStatement		compile asm
syn keyword fxType		string texture technique pass

" syn match fxCast		"\<\(const\|static\|dynamic\|reinterpret\)_cast\s*<"me=e-1
" syn match fxCast		"\<\(const\|static\|dynamic\|reinterpret\)_cast\s*$"
syn match fxSwizzle		/\.[xyzw]\{1,4\}/
syn match fxSwizzle		/\.[rgba]\{1,4\}/
syn match fxSwizzle		/\.\(_m[0-3]\{2}\)\{1,4\}/
syn match fxSwizzle		/\.\(_[1-4]\{2}\)\{1,4\}/
syn match fxSemantic		/:\s*[A-Z]\w*/
syn keyword fxStorageClass	in out inout uniform packed const
syn keyword fxNumber	NPOS
"syn keyword fxBoolean	true false none
syn match fxBoolean	/\<\c\(true\|false\|none\)\>/

" The minimum and maximum operators in GNU C++
syn match fxMinMax "[<>]?"

" Default highlighting
if version >= 508 || !exists("did_fx_syntax_inits")
  if version < 508
    let did_fx_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  " HiLink fxCast			fxStatement
  HiLink fxStatement		Statement
  HiLink fxProfile		fxStatement
  HiLink fxSamplerArg		fxStatement
  HiLink fxType			Type
  HiLink fxType			Type
  HiLink fxStorageClass		StorageClass
  HiLink fxSemantic		Structure
  HiLink fxNumber		Number
  highlight fxAnnotation	ctermfg=red guifg=red
  highlight fxSwizzle		ctermfg=magenta guifg=magenta
  HiLink fxBoolean		Boolean
  delcommand HiLink
endif

"make compatible with VS.Net and FXComposer...
"set ts=4

let b:current_syntax = "fx"

"
" ABBREVIATIONS FOR FX FILES
"

" Float tweakables of various sorts
iabbrev fl! float Name : POWER <
iabbrev f1! float Name : POWER <
iabbrev f2! float2 Name <
iabbrev f3! float3 Name <
iabbrev f4! float4 Name <

" 4x4 matrix with identity
iabbrev xf! float4x4 Name = {1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};

" color tweakables as float3 or float4
iabbrev c3! float3 Name : DIFFUSE <
iabbrev c4! float4 Name : DIFFUSE <

" direction tweakables as float3 or float4
iabbrev d3! float3 Name : DIRECTION <
iabbrev d4! float4 Name : DIRECTION <

" position tweakables as float3 or float4
iabbrev p3! float3 Name : POSITION <
iabbrev p4! float4 Name : POSITION <

" standard set of "untweakables"
iabbrev ut! float4x4 WorldITXf : WorldInverseTranspose = {1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};

" simple pass (within a technique)
iabbrev ps! pass PassName {

" application data struct declaration
iabbrev ad! /* data from application vertex buffer */

" vertex data struct declaration
iabbrev vd! struct vertexOutput {

" file-based 2D texture and sampler declaration
iabbrev tx! texture NamedTexture : DiffuseMap <

" render-to-texture texture and sampler declaration
iabbrev rtx! #define RTT_SIZE 256

" declaration of data structure and vertex shader for using "fullscreenquad" geometry
iabbrev vq! struct rttOut

" render-to-texture pass declarations (within technique) -- create and use
iabbrev rtp! pass WriteBuffer <

" FX-generated 2D texture, sampler, and template function
iabbrev gtx! #define GEN_TEX_SIZE 64

" ambient light color
iabbrev la! float3 AmbiLightColor : AMBIENT

" Directional Light Parameters
iabbrev ld! // Directional Light 1 ////

" Point light parameters
iabbrev lp! // Point Light 1 ////

" vim: ts=8