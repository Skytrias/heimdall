package nanovg_gl

import "core:log"
import "core:strings"
import "core:mem"
import "core:math"
import "core:fmt"
import gl "vendor:OpenGL"
import nvg "../nanovg"

Color :: nvg.Color
Vertex :: nvg.Vertex
Image_Flags :: nvg.Image_Flags
Texture_Type :: nvg.Texture_Type
Paint :: nvg.Paint
scissor :: nvg.scissor

Create_Flag :: enum {
	// Flag indicating if geometry based anti-aliasing is used (may not be needed when using MSAA).
	Anti_Alias,
	// Flag indicating if strokes should be drawn using stencil buffer. The rendering will be a little
	// slower, but path overlaps (i.e. self-intersecting or sharp turns) will be drawn just once.
	Stencil_Strokes,
	// additional debug checks
	Debug,
}
Create_Flags :: bit_set[Create_Flag]

// TODO switch between GL versions

FRAG_BINDING :: 0
USE_STATE_FILTER :: true
GL_UNIFORMARRAY_SIZE :: 11

Uniform_Loc :: enum {
	View_Size,
	Tex,
	Frag,
}

Shader_Type :: enum i32 {
	Fill_Grad,
	Fill_Img,
	Simple,
	Img,
}

Shader :: struct {
	prog: u32,
	frag: u32,
	vert: u32,
	loc: [Uniform_Loc]i32,
}

Texture :: struct {
	id: int,
	tex: u32,
	width, height: int,
	type: Texture_Type,
	flags: Image_Flags,
}

Blend :: struct {
	src_RGB: u32,
	dst_RGB: u32,
	src_alpha: u32,
	dst_alpha: u32,
}

Call_Type :: enum {
	None,
	Fill,
	Convex_Fill,
	Stroke,
	Triangles,
}

Call :: struct {
	type: Call_Type,
	image: int,
	path_offset: int,
	path_count: int,
	triangle_offset: int,
	triangle_count: int,
	uniform_offset: int,
	blend_func: Blend,
}

Path :: struct {
	fill_offset: int,
	fill_count: int,
	stroke_offset: int,
	stroke_count: int,
}

when GL2_IMPLEMENTATION {
	Frag_Uniforms :: struct #raw_union {
		using _: struct {
			scissor_mat: [12]f32, // matrices are actually 3 vec4s
			paint_mat: [12]f32,
			inner_color: Color,
			outer_color: Color,
			scissor_ext: [2]f32,
			scissor_scale: [2]f32,
			extent: [2]f32,
			radius: f32,
			feather: f32,
			stroke_mult: f32,
			stroke_thr: f32,
			tex_type: i32,
			type: Shader_Type,
		},
		uniform_array: [GL_UNIFORMARRAY_SIZE][4]f32,
	}
} else {
	Frag_Uniforms :: struct #packed {
		scissor_mat: [12]f32, // matrices are actually 3 vec4s
		paint_mat: [12]f32,
		inner_color: Color,
		outer_color: Color,
		scissor_ext: [2]f32,
		scissor_scale: [2]f32,
		extent: [2]f32,
		radius: f32,
		feather: f32,
		stroke_mult: f32,
		stroke_thr: f32,
		tex_type: i32,
		type: Shader_Type,
	}
}

GL2_IMPLEMENTATION :: false
GL3_IMPLEMENTATION :: true
GLES2_IMPLEMENTATION :: false
GLES3_IMPLEMENTATION :: false

when GL2_IMPLEMENTATION {
	GL2 :: true
	GL3 :: false
	GLES2 :: false
	GLES3 :: false
	GL_IMPLEMENTATION :: true
	GL_USE_UNIFORMBUFFER :: false
} else when GL3_IMPLEMENTATION {
	GL2 :: false
	GL3 :: true
	GLES2 :: false
	GLES3 :: false
	GL_IMPLEMENTATION :: true
	GL_USE_UNIFORMBUFFER :: true
} else when GLES2_IMPLEMENTATION {
	GL2 :: false
	GL3 :: false
	GLES2 :: true
	GLES3 :: false
	GL_IMPLEMENTATION :: true
	GL_USE_UNIFORMBUFFER :: false
} else when GLES3_IMPLEMENTATION {
	GL2 :: false
	GL3 :: false
	GLES2 :: false
	GLES3 :: true
	GL_IMPLEMENTATION :: true
	GL_USE_UNIFORMBUFFER :: false
}

Context :: struct {
	shader: Shader,
	textures: [dynamic]Texture,
	view: [2]f32,
	texture_id: int,

	vert_buf: u32,
	vert_arr: u32, // GL3
	frag_buf: u32, // USE_UNIFORMBUFFER
	frag_size: int,
	flags: Create_Flags,

	// Per frame buffers
	calls: [dynamic]Call,
	paths: [dynamic]Path,
	verts: [dynamic]Vertex,
	uniforms: [dynamic]byte,

	// cached state used for state filter
	bound_texture: u32,
	stencil_mask: u32,
	stencil_func: u32,
	stencil_func_ref: i32,
	stencil_func_mask: u32,
	blend_func: Blend,

	dummy_tex: int,
}

__nearestPow2 :: proc(num: uint) -> uint {
	n := num > 0 ? num - 1 : 0
	n |= n >> 1
	n |= n >> 2
	n |= n >> 4
	n |= n >> 8
	n |= n >> 16
	n += 1
	return n
}

__bindTexture :: proc(ctx: ^Context, tex: u32) {
	when USE_STATE_FILTER {
		if ctx.bound_texture != tex {
			ctx.bound_texture = tex
			gl.BindTexture(gl.TEXTURE_2D, tex)
		}
	} else {
		gl.BindTexture(gl.TEXTURE_2D, tex)
	}
}

__stencilMask :: proc(ctx: ^Context, mask: u32) {
	when USE_STATE_FILTER {
		if ctx.stencil_mask != mask {
			ctx.stencil_mask = mask
			gl.StencilMask(mask)
		}
	} else {
		gl.StencilMask(mask)
	}
}

__stencilFunc :: proc(ctx: ^Context, func: u32, ref: i32, mask: u32) {
	when USE_STATE_FILTER {
		if ctx.stencil_func != func ||
			ctx.stencil_func_ref != ref ||
			ctx.stencil_func_mask != mask {
			ctx.stencil_func = func
			ctx.stencil_func_ref = ref
			ctx.stencil_func_mask = mask
			gl.StencilFunc(func, ref, mask)
		}
	} else {
		gl.StencilFunc(func, ref, mask)
	}
}

__blendFuncSeparate :: proc(ctx: ^Context, blend: ^Blend) {
	when USE_STATE_FILTER {
		if ctx.blend_func != blend^ {
			ctx.blend_func = blend^
			gl.BlendFuncSeparate(blend.src_RGB, blend.dst_RGB, blend.src_alpha, blend.dst_alpha)
		}
	} else {
		gl.BlendFuncSeparate(blend.src_RGB, blend.dst_RGB, blend.src_alpha, blend.dst_alpha)
	}
}

__allocTexture :: proc(ctx: ^Context) -> (tex: ^Texture) {
	for texture in &ctx.textures {
		if texture.id == 0 {
			tex = &texture
			break
		}
	}

	if tex == nil {
		append(&ctx.textures, Texture {})
		tex = &ctx.textures[len(ctx.textures) - 1]
	}

	tex^ = {}
	ctx.texture_id += 1
	tex.id = ctx.texture_id

	return
}

__findTexture :: proc(ctx: ^Context, id: int) -> ^Texture {
	for texture in &ctx.textures {
		if texture.id == id {
			return &texture
		}
	}

	return nil
}

__deleteTexture :: proc(ctx: ^Context, id: int) -> bool {
	for texture, i in &ctx.textures {
		if texture.id == id {
			if texture.tex != 0 && (.No_Delete not_in texture.flags) {
				gl.DeleteTextures(1, &texture.tex)
			}

			ctx.textures[i] = {}
			return true
		}
	}

	return false
}

__deleteShader :: proc(shader: ^Shader) {
	if shader.prog != 0 {
		gl.DeleteProgram(shader.prog)
	}

	if shader.vert != 0 {
		gl.DeleteShader(shader.vert)
	}

	if shader.frag != 0 {
		gl.DeleteShader(shader.frag)
	}
}

__getUniforms :: proc(shader: ^Shader) {
	shader.loc[.View_Size] = gl.GetUniformLocation(shader.prog, "viewSize")
	shader.loc[.Tex] = gl.GetUniformLocation(shader.prog, "tex")
	
	when GL_USE_UNIFORMBUFFER {
		shader.loc[.Frag] = i32(gl.GetUniformBlockIndex(shader.prog, "frag"))
	} else {
		shader.loc[.Frag] = gl.GetUniformLocation(shader.prog, "frag")
	}
}

vert_shader := #load("vert.glsl")
frag_shader := #load("frag.glsl")

__renderCreate :: proc(uptr: rawptr) -> bool {
	ctx := cast(^Context) uptr

	// just build the string at runtime
	builder := strings.builder_make(0, 512, context.temp_allocator)

	when GL2 {
		strings.write_string(&builder, "#define NANOVG_GL2 1\n")
	} else when GL3 {
		strings.write_string(&builder, "#version 150 core\n#define NANOVG_GL3 1\n")
	} else when GLES2 {
		strings.write_string(&builder, "#version 100\n#define NANOVG_GL2 1\n")
	} else when GLES3 {
		strings.write_string(&builder, "#version 300 es\n#define NANOVG_GL3 1\n")
	}

	when GL_USE_UNIFORMBUFFER {
		strings.write_string(&builder, "#define USE_UNIFORMBUFFER 1\n")
	} else {
		strings.write_string(&builder, "#define UNIFORMARRAY_SIZE 11\n")
	} 

	__checkError(ctx, "init")

	shader_header := strings.to_string(builder)
	anti: string = .Anti_Alias in ctx.flags ? "#define EDGE_AA 1\n" : " "
	if !__createShader(
		&ctx.shader, 
		shader_header,
		anti, 
		string(vert_shader),
		string(frag_shader),
	) {
		return false
	}

	__checkError(ctx, "uniform locations")
	__getUniforms(&ctx.shader)

	when GL3 {
		gl.GenVertexArrays(1, &ctx.vert_arr)
	} 

	gl.GenBuffers(1, &ctx.vert_buf)
	align := i32(4)

	when GL_USE_UNIFORMBUFFER {
		// Create UBOs
		gl.UniformBlockBinding(ctx.shader.prog, u32(ctx.shader.loc[.Frag]), FRAG_BINDING)
		gl.GenBuffers(1, &ctx.frag_buf)
		gl.GetIntegerv(gl.UNIFORM_BUFFER_OFFSET_ALIGNMENT, &align)
	} 

	ctx.frag_size = int(size_of(Frag_Uniforms) + align - size_of(Frag_Uniforms) % align)
	// ctx.frag_size = size_of(Frag_Uniforms)
	ctx.dummy_tex = __renderCreateTexture(ctx, .Alpha, 1, 1, {}, nil)

	__checkError(ctx, "create done")
	
	gl.Finish()

	return true
}

__renderCreateTexture :: proc(
	uptr: rawptr, 
	type: Texture_Type, 
	w, h: int, 
	image_flags: Image_Flags,
	data: []byte,
) -> int {
	ctx := cast(^Context) uptr
	tex := __allocTexture(ctx)
	image_flags := image_flags

	if tex == nil {
		return 0
	}

	when GLES2 {
		if __nearestPow2(uint(w)) != uint(w) || __nearestPow2(uint(h)) != uint(h) {
			// No repeat
			if (.Repeat_X in image_flags) || (.Repeat_Y in image_flags) {
				log.errorf("Repeat X/Y is not supported for non power-of-two textures (%d x %d)\n", w, h)
				excl(&image_flags, Image_Flags { .Repeat_X, .Repeat_Y })
			}

			// No mips.
			if .Generate_Mipmaps in image_flags {
				log.errorf("Mip-maps is not support for non power-of-two textures (%d x %d)\n", w, h);
				excl(&image_flags, nvg.Image_Flag.Generate_Mipmaps)
			}
		}
	}

	gl.GenTextures(1, &tex.tex)
	tex.width = w
	tex.height = h
	tex.type = type
	tex.flags = image_flags
	__bindTexture(ctx, tex.tex)

	gl.PixelStorei(gl.UNPACK_ALIGNMENT,1)
	
	when GLES2 {
		gl.PixelStorei(gl.UNPACK_ROW_LENGTH, i32(tex.width))
		gl.PixelStorei(gl.UNPACK_SKIP_PIXELS, 0)
		gl.PixelStorei(gl.UNPACK_SKIP_ROWS, 0)
	}

	when GL2 {
		if .Generate_Mipmaps in image_flags {
			gl.TexParameteri(gl.TEXTURE_2D, GENERATE_MIPMAP, 1)
		}
	}

	if type == .RGBA {
		gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, i32(w), i32(h), 0, gl.RGBA, gl.UNSIGNED_BYTE, raw_data(data))
	} else {
		when GLES2 || GL2 {
			// TODO missing in odin
			gl.TexImage2D(gl.TEXTURE_2D, 0, gl.LUMINANCE, w, h, 0, gl.LUMINANCE, gl.UNSIGNED_BYTE, data)
		} else when GLES3 {
			gl.TexImage2D(gl.TEXTURE_2D, 0, gl.R8, i32(w), i32(h), 0, gl.RED, gl.UNSIGNED_BYTE, raw_data(data))
		} else {
			gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RED, i32(w), i32(h), 0, gl.RED, gl.UNSIGNED_BYTE, raw_data(data))
		}
	}

	if .Generate_Mipmaps in image_flags {
		if .Nearest in image_flags {
			gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST_MIPMAP_NEAREST)
		} else {
			gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
		}
	} else {
		if .Nearest in image_flags {
			gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
		} else {
			gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
		}
	}

	if .Nearest in image_flags {
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
	} else {
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	}

	if .Repeat_X in image_flags {
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT)
	}	else {
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
	}

	if .Repeat_Y in image_flags {
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT)
	}	else {
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
	}

	gl.PixelStorei(gl.UNPACK_ALIGNMENT, 4)

	when GLES2 {
		gl.PixelStorei(gl.UNPACK_ROW_LENGTH, 0)
		gl.PixelStorei(gl.UNPACK_SKIP_PIXELS, 0)
		gl.PixelStorei(gl.UNPACK_SKIP_ROWS, 0)
	}

	// The new way to build mipmaps on GLES and GL3
	when !GL2 {
		if .Generate_Mipmaps in image_flags {
			gl.GenerateMipmap(gl.TEXTURE_2D)
		}
	}

	__checkError(ctx, "create tex")
	__bindTexture(ctx, 0)

	return tex.id
}

__checkError :: proc(ctx: ^Context, str: string) {
	if .Debug in ctx.flags {
		err := gl.GetError()

		if err != gl.NO_ERROR {
			log.errorf("FOUND ERROR %08x:\n\t%s\n", err, str)
		}
	}
}

__checkProgramError :: proc(prog: u32) {
	status: i32
	gl.GetProgramiv(prog, gl.LINK_STATUS, &status)
	length: i32
	gl.GetProgramiv(prog, gl.INFO_LOG_LENGTH, &length)

	if status == 0 {
		temp := make([]byte, length)
		defer delete(temp)

		gl.GetProgramInfoLog(prog, length, nil, raw_data(temp))
		log.errorf("Program Error:\n%s\n", string(temp[:length]))
	}
}

__checkShaderError :: proc(shader: u32, type: string) {
	status: i32
	gl.GetShaderiv(shader, gl.COMPILE_STATUS, &status)
	length: i32
	gl.GetShaderiv(shader, gl.INFO_LOG_LENGTH, &length)

	if status == 0 {
		temp := make([]byte, length)
		defer delete(temp)

		gl.GetShaderInfoLog(shader, length, nil, raw_data(temp))
		log.errorf("Shader error:\n%s\n", string(temp[:length]))	
	}
}

// TODO good case for or_return
__createShader :: proc(
	shader: ^Shader,
	header: string,
	opts: string,
	vshader: string,
	fshader: string,
) -> bool {
	shader^ = {}
	str: [3]cstring
	lengths: [3]i32
	str[0] = cstring(raw_data(header))
	str[1] = cstring(raw_data(opts))

	lengths[0] = i32(len(header))
	lengths[1] = i32(len(opts))

	prog := gl.CreateProgram()
	vert := gl.CreateShader(gl.VERTEX_SHADER)
	frag := gl.CreateShader(gl.FRAGMENT_SHADER)
	
	// vert shader
	str[2] = cstring(raw_data(vshader))
	lengths[2] = i32(len(vshader))
	gl.ShaderSource(vert, 3, &str[0], &lengths[0])
	gl.CompileShader(vert)
	__checkShaderError(vert, "vert")
	
	// fragment shader
	str[2] = cstring(raw_data(fshader))
	lengths[2] = i32(len(fshader))
	gl.ShaderSource(frag, 3, &str[0], &lengths[0])
	gl.CompileShader(frag)
	__checkShaderError(frag, "frag")

	gl.AttachShader(prog, vert)
	gl.AttachShader(prog, frag)

	gl.BindAttribLocation(prog, 0, "vertex")
	gl.BindAttribLocation(prog, 1, "tcoord")

	gl.LinkProgram(prog)
	__checkProgramError(prog)

	shader.prog = prog
	shader.vert = vert
	shader.frag = frag
	return true
}

__renderDeleteTexture :: proc(uptr: rawptr, image: int) -> bool {
	ctx := cast(^Context) uptr
	return __deleteTexture(ctx, image)
}

__renderUpdateTexture :: proc(
	uptr: rawptr, 
	image: int,
	x, y: int,
	w, h: int,
	data: []byte,
) -> bool {
	ctx := cast(^Context) uptr
	tex := __findTexture(ctx, image)

	if tex == nil {
		return false
	}

	__bindTexture(ctx, tex.tex)

	gl.PixelStorei(gl.UNPACK_ALIGNMENT,1)

	x := x
	w := w
	data := data

	when GLES2 {
		gl.PixelStorei(gl.UNPACK_ROW_LENGTH, i32(tex.width))
		gl.PixelStorei(gl.UNPACK_SKIP_PIXELS, i32(x))
		gl.PixelStorei(gl.UNPACK_SKIP_ROWS, i32(y))
	} else {
		// No support for all of skip, need to update a whole row at a time.
		if tex.type == .RGBA {
			data = data[y * tex.width * 4:]
		}	else {
			data = data[y * tex.width:]
		}

		x = 0
		w = tex.width
	}

	if tex.type == .RGBA {
		gl.TexSubImage2D(gl.TEXTURE_2D, 0, i32(x), i32(y), i32(w), i32(h), gl.RGBA, gl.UNSIGNED_BYTE, raw_data(data))
	} else {
		when GLES2 || GL2 {
			// TODO is missing in odin
			// gl.TexSubImage2D(gl.TEXTURE_2D, 0, i32(x), i32(y), i32(w), i32(h), gl.LUMINANCE, gl.UNSIGNED_BYTE, raw_data(data))
		} else {
			gl.TexSubImage2D(gl.TEXTURE_2D, 0, i32(x), i32(y), i32(w), i32(h), gl.RED, gl.UNSIGNED_BYTE, raw_data(data))
		}
	}

	gl.PixelStorei(gl.UNPACK_ALIGNMENT, 4)

	when GLES2 {
		gl.PixelStorei(gl.UNPACK_ROW_LENGTH, 0)
		gl.PixelStorei(gl.UNPACK_SKIP_PIXELS, 0)
		gl.PixelStorei(gl.UNPACK_SKIP_ROWS, 0)
	}

	__bindTexture(ctx, 0)

	return true
}

__renderGetTextureSize :: proc(uptr: rawptr, image: int, w, h: ^int) -> bool {
	ctx := cast(^Context) uptr
	tex := __findTexture(ctx, image)

	if tex == nil {
		return false
	}

	w^ = tex.width
	h^ = tex.height
	return true
}

__xformToMat3x4 :: proc(m3: ^[12]f32, t: [6]f32) {
	m3[0] = t[0]
	m3[1] = t[1]
	m3[2] = 0
	m3[3] = 0
	m3[4] = t[2]
	m3[5] = t[3]
	m3[6] = 0
	m3[7] = 0
	m3[8] = t[4]
	m3[9] = t[5]
	m3[10] = 1
	m3[11] = 0
}

__premulColor :: proc(c: Color) -> (res: Color) {
	res = c
	res.r *= c.a
	res.g *= c.a
	res.b *= c.a
	return
}

__convertPaint :: proc(
	ctx: ^Context,
	frag: ^Frag_Uniforms,
	paint: ^Paint,
	scissor: ^scissor,
	width: f32,
	fringe: f32,
	stroke_thr: f32,
) -> bool {
	invxform: [6]f32
	frag^ = {}
	frag.inner_color = __premulColor(paint.inner_color)
	frag.outer_color = __premulColor(paint.outer_color)

	if scissor.extent[0] < -0.5 || scissor.extent[1] < -0.5 {
		frag.scissor_mat = {}
		frag.scissor_ext[0] = 1.0
		frag.scissor_ext[1] = 1.0
		frag.scissor_scale[0] = 1.0
		frag.scissor_scale[1] = 1.0
	} else {
		nvg.TransformInverse(&invxform, scissor.xform)
		__xformToMat3x4(&frag.scissor_mat, invxform)
		frag.scissor_ext[0] = scissor.extent[0]
		frag.scissor_ext[1] = scissor.extent[1]
		frag.scissor_scale[0] = math.sqrt(scissor.xform[0]*scissor.xform[0] + scissor.xform[2]*scissor.xform[2]) / fringe
		frag.scissor_scale[1] = math.sqrt(scissor.xform[1]*scissor.xform[1] + scissor.xform[3]*scissor.xform[3]) / fringe
	}

	frag.extent = paint.extent
	frag.stroke_mult = (width * 0.5 + fringe * 0.5) / fringe
	frag.stroke_thr = stroke_thr

	if paint.image != 0 {
		tex := __findTexture(ctx, paint.image)
		
		if tex == nil {
			return false
		}
		
		// TODO maybe inversed?
		if .Flip_Y in tex.flags {
			m1: [6]f32
			m2: [6]f32
			nvg.TransformTranslate(&m1, 0.0, frag.extent[1] * 0.5)
			nvg.TransformMultiply(&m1, paint.xform)
			nvg.TransformScale(&m2, 1.0, -1.0)
			nvg.TransformMultiply(&m2, m1)
			nvg.TransformTranslate(&m1, 0.0, -frag.extent[1] * 0.5)
			nvg.TransformMultiply(&m1, m2)
			nvg.TransformInverse(&invxform, m1)
		} else {
			nvg.TransformInverse(&invxform, paint.xform)
		}

		frag.type = .Fill_Img

		when GL_USE_UNIFORMBUFFER {
			if tex.type == .RGBA {
				frag.tex_type = (.Premultiplied in tex.flags) ? 0 : 1
			}	else {
				frag.tex_type = 2
			}
		} else {
			if tex.type == .RGBA {
				frag.tex_type = (.Premultiplied in tex.flags) ? 0.0 : 1.0
			}	else {
				frag.tex_type = 2.0
			}
		}
	} else {
		frag.type = .Fill_Grad
		frag.radius = paint.radius
		frag.feather = paint.feather
		nvg.TransformInverse(&invxform, paint.xform)
	}

	__xformToMat3x4(&frag.paint_mat, invxform)

	return true
}

__setUniforms :: proc(ctx: ^Context, uniform_offset: int, image: int) {
	when GL_USE_UNIFORMBUFFER {
		gl.BindBufferRange(gl.UNIFORM_BUFFER, FRAG_BINDING, ctx.frag_buf, uniform_offset, size_of(Frag_Uniforms))
	} else {
		frag := __fragUniformPtr(ctx, uniform_offset)
		gl.Uniform4fv(ctx.shader.loc[.Frag], GL_UNIFORMARRAY_SIZE, cast(^f32) frag)
	}

	__checkError(ctx, "uniform4")

	tex: ^Texture
	if image != 0 {
		tex = __findTexture(ctx, image)
	}
	
	// If no image is set, use empty texture
	if tex == nil {
		tex = __findTexture(ctx, ctx.dummy_tex)
	}

	__bindTexture(ctx, tex != nil ? tex.tex : 0)
	__checkError(ctx, "tex paint tex")
}

__renderViewport :: proc(uptr: rawptr, width, height, device_pixel_ratio: f32) {
	ctx := cast(^Context) uptr
	ctx.view[0] = width
	ctx.view[1] = height
}

__fill :: proc(ctx: ^Context, call: ^Call) {
	paths := ctx.paths[call.path_offset:]

	// Draw shapes
	gl.Enable(gl.STENCIL_TEST)
	__stencilMask(ctx, 0xff)
	__stencilFunc(ctx, gl.ALWAYS, 0, 0xff)
	gl.ColorMask(gl.FALSE, gl.FALSE, gl.FALSE, gl.FALSE)

	// set bindpoint for solid loc
	__setUniforms(ctx, call.uniform_offset, 0)
	__checkError(ctx, "fill simple")

	gl.StencilOpSeparate(gl.FRONT, gl.KEEP, gl.KEEP, gl.INCR_WRAP)
	gl.StencilOpSeparate(gl.BACK, gl.KEEP, gl.KEEP, gl.DECR_WRAP)
	gl.Disable(gl.CULL_FACE)
	for i in 0..<call.path_count {
		gl.DrawArrays(gl.TRIANGLE_FAN, i32(paths[i].fill_offset), i32(paths[i].fill_count))
	}
	gl.Enable(gl.CULL_FACE)

	// Draw anti-aliased pixels
	gl.ColorMask(gl.TRUE, gl.TRUE, gl.TRUE, gl.TRUE)

	__setUniforms(ctx, call.uniform_offset + ctx.frag_size, call.image)
	__checkError(ctx, "fill fill")

	if .Anti_Alias in ctx.flags {
		__stencilFunc(ctx, gl.EQUAL, 0x00, 0xff)
		gl.StencilOp(gl.KEEP, gl.KEEP, gl.KEEP)
		// Draw fringes
		for i in 0..<call.path_count {
			gl.DrawArrays(gl.TRIANGLE_STRIP, i32(paths[i].stroke_offset), i32(paths[i].stroke_count))
		}
	}

	// Draw fill
	__stencilFunc(ctx, gl.NOTEQUAL, 0x0, 0xff)
	gl.StencilOp(gl.ZERO, gl.ZERO, gl.ZERO)
	gl.DrawArrays(gl.TRIANGLE_STRIP, i32(call.triangle_offset), i32(call.triangle_count))

	gl.Disable(gl.STENCIL_TEST)
}

__convexFill :: proc(ctx: ^Context, call: ^Call) {
	paths := ctx.paths[call.path_offset:]

	__setUniforms(ctx, call.uniform_offset, call.image)
	__checkError(ctx, "convex fill")

	for i in 0..<call.path_count {
		gl.DrawArrays(gl.TRIANGLE_FAN, i32(paths[i].fill_offset), i32(paths[i].fill_count))
	
		// draw fringes
		if paths[i].stroke_count > 0 {
			gl.DrawArrays(gl.TRIANGLE_STRIP, i32(paths[i].stroke_offset), i32(paths[i].stroke_count))
		}
	}
}

__stroke :: proc(ctx: ^Context, call: ^Call) {
	paths := ctx.paths[call.path_offset:]

	if .Stencil_Strokes in ctx.flags {
		gl.Enable(gl.STENCIL_TEST)
		__stencilMask(ctx, 0xff)

		// Fill the stroke base without overlap
		__stencilFunc(ctx, gl.EQUAL, 0x0, 0xff)
		gl.StencilOp(gl.KEEP, gl.KEEP, gl.INCR)
		__setUniforms(ctx, call.uniform_offset + ctx.frag_size, call.image)
		__checkError(ctx, "stroke fill 0")
		
		for i in 0..<call.path_count {
			gl.DrawArrays(gl.TRIANGLE_STRIP, i32(paths[i].stroke_offset), i32(paths[i].stroke_count))
		}

		// Draw anti-aliased pixels.
		__setUniforms(ctx, call.uniform_offset, call.image)
		__stencilFunc(ctx, gl.EQUAL, 0x00, 0xff)
		gl.StencilOp(gl.KEEP, gl.KEEP, gl.KEEP)
		for i in 0..<call.path_count {
			gl.DrawArrays(gl.TRIANGLE_STRIP, i32(paths[i].stroke_offset), i32(paths[i].stroke_count))
		}

		// Clear stencil buffer.
		gl.ColorMask(gl.FALSE, gl.FALSE, gl.FALSE, gl.FALSE)
		__stencilFunc(ctx, gl.ALWAYS, 0x0, 0xff)
		gl.StencilOp(gl.ZERO, gl.ZERO, gl.ZERO)
		__checkError(ctx, "stroke fill 1")
		for i in 0..<call.path_count {
			gl.DrawArrays(gl.TRIANGLE_STRIP, i32(paths[i].stroke_offset), i32(paths[i].stroke_count))
		}
		gl.ColorMask(gl.TRUE, gl.TRUE, gl.TRUE, gl.TRUE)

		gl.Disable(gl.STENCIL_TEST)
	} else {
		__setUniforms(ctx, call.uniform_offset, call.image)
		__checkError(ctx, "stroke fill")
		
		// Draw Strokes
		for i in 0..<call.path_count {
			gl.DrawArrays(gl.TRIANGLE_STRIP, i32(paths[i].stroke_offset), i32(paths[i].stroke_count))
		}
	}
}

__triangles :: proc(ctx: ^Context, call: ^Call) {
	__setUniforms(ctx, call.uniform_offset, call.image)
	__checkError(ctx, "triangles fill")
	gl.DrawArrays(gl.TRIANGLES, i32(call.triangle_offset), i32(call.triangle_count))
}

__renderCancel :: proc(uptr: rawptr) {
	ctx := cast(^Context) uptr
	clear(&ctx.verts)
	clear(&ctx.paths)
	clear(&ctx.calls)
	clear(&ctx.uniforms)
}

BLEND_FACTOR_TABLE :: [nvg.Blend_Factor]u32 {
	.ZERO = gl.ZERO,
	.ONE = gl.ONE,
	.SRC_COLOR = gl.SRC_COLOR,
	.ONE_MINUS_SRC_COLOR = gl.ONE_MINUS_SRC_COLOR,
	.DST_COLOR = gl.DST_COLOR,
	.ONE_MINUS_DST_COLOR = gl.ONE_MINUS_DST_COLOR,
	.SRC_ALPHA = gl.SRC_ALPHA,
	.ONE_MINUS_SRC_ALPHA = gl.ONE_MINUS_SRC_ALPHA,
	.DST_ALPHA = gl.DST_ALPHA,
	.ONE_MINUS_DST_ALPHA = gl.ONE_MINUS_DST_ALPHA,
	.SRC_ALPHA_SATURATE = gl.SRC_ALPHA_SATURATE,
}

__blendCompositeOperation :: proc(op: nvg.Composite_Operation_State) -> Blend {
	table := BLEND_FACTOR_TABLE
	blend := Blend {
		table[op.src_RGB],
		table[op.dst_RGB],
		table[op.src_alpha],
		table[op.dst_alpha],
	}
	return blend
}

__renderFlush :: proc(uptr: rawptr) {
	ctx := cast(^Context) uptr

	if len(ctx.calls) > 0 {
		// Setup require GL state.
		gl.UseProgram(ctx.shader.prog)

		gl.Enable(gl.CULL_FACE)
		gl.CullFace(gl.BACK)
		gl.FrontFace(gl.CCW)
		gl.Enable(gl.BLEND)
		gl.Disable(gl.DEPTH_TEST)
		gl.Disable(gl.SCISSOR_TEST)
		gl.ColorMask(gl.TRUE, gl.TRUE, gl.TRUE, gl.TRUE)
		gl.StencilMask(0xffffffff)
		gl.StencilOp(gl.KEEP, gl.KEEP, gl.KEEP)
		gl.StencilFunc(gl.ALWAYS, 0, 0xffffffff)
		gl.ActiveTexture(gl.TEXTURE0)
		gl.BindTexture(gl.TEXTURE_2D, 0)
		
		when USE_STATE_FILTER {
			ctx.bound_texture = 0
			ctx.stencil_mask = 0xffffffff
			ctx.stencil_func = gl.ALWAYS
			ctx.stencil_func_ref = 0
			ctx.stencil_func_mask = 0xffffffff
			ctx.blend_func.src_RGB = gl.INVALID_ENUM
			ctx.blend_func.src_alpha = gl.INVALID_ENUM
			ctx.blend_func.dst_RGB = gl.INVALID_ENUM
			ctx.blend_func.dst_alpha = gl.INVALID_ENUM
		}

		when GL_USE_UNIFORMBUFFER {
			// Upload ubo for frag shaders
			gl.BindBuffer(gl.UNIFORM_BUFFER, ctx.frag_buf)
			gl.BufferData(gl.UNIFORM_BUFFER, len(ctx.uniforms), raw_data(ctx.uniforms), gl.STREAM_DRAW)
		}

		// Upload vertex data
		when GL3 {
			gl.BindVertexArray(ctx.vert_arr)
		}

		gl.BindBuffer(gl.ARRAY_BUFFER, ctx.vert_buf)
		gl.BufferData(gl.ARRAY_BUFFER, len(ctx.verts) * size_of(Vertex), raw_data(ctx.verts), gl.STREAM_DRAW)
		gl.EnableVertexAttribArray(0)
		gl.EnableVertexAttribArray(1)
		gl.VertexAttribPointer(0, 2, gl.FLOAT, gl.FALSE, size_of(Vertex), 0)
		gl.VertexAttribPointer(1, 2, gl.FLOAT, gl.FALSE, size_of(Vertex), 2 * size_of(f32))

		// Set view and texture just once per frame.
		gl.Uniform1i(ctx.shader.loc[.Tex], 0)
		gl.Uniform2fv(ctx.shader.loc[.View_Size], 1, &ctx.view[0])

		when GL_USE_UNIFORMBUFFER {
			gl.BindBuffer(gl.UNIFORM_BUFFER, ctx.frag_buf)
		}

		for i in 0..<len(ctx.calls) {
			call := &ctx.calls[i]
			__blendFuncSeparate(ctx, &call.blend_func)

			switch call.type {
				case .None: {}
				case .Fill: __fill(ctx, call)
				case .Convex_Fill: __convexFill(ctx, call)
				case .Stroke: __stroke(ctx, call)
				case .Triangles: __triangles(ctx, call)
			}
		}

		gl.DisableVertexAttribArray(0)
		gl.DisableVertexAttribArray(1)

		when GL3 {
			gl.BindVertexArray(0)
		}

		gl.Disable(gl.CULL_FACE)
		gl.BindBuffer(gl.ARRAY_BUFFER, 0)
		gl.UseProgram(0)
		__bindTexture(ctx, 0)
	}

	// Reset calls
	clear(&ctx.verts)
	clear(&ctx.paths)
	clear(&ctx.calls)
	clear(&ctx.uniforms)
}

__maxVertCount :: proc(paths: []nvg.Path) -> (count: int) {
	for i in 0..<len(paths) {
		count += len(paths[i].fill)
		count += len(paths[i].stroke)
	}
	return
}

__allocCall :: #force_inline proc(ctx: ^Context) -> ^Call {
	append(&ctx.calls, Call {})
	return &ctx.calls[len(ctx.calls) - 1]
}

// alloc paths and return the original start position
__allocPaths :: proc(ctx: ^Context, count: int) -> int {
	old := len(ctx.paths)
	resize(&ctx.paths, len(ctx.paths) + count)
	return old
}

// alloc verts and return the original start position
__allocVerts :: proc(ctx: ^Context, count: int) -> int {
	old := len(ctx.verts)
	resize(&ctx.verts, len(ctx.verts) + count)
	return old
}

// alloc uniforms and return the original start position
__allocFragUniforms :: proc(ctx: ^Context, count: int) -> int {
	ret := len(ctx.uniforms)
	resize(&ctx.uniforms, len(ctx.uniforms) + count * ctx.frag_size)
	return ret
}

// get frag uniforms from byte slice offset
__fragUniformPtr :: proc(ctx: ^Context, offset: int) -> ^Frag_Uniforms {
	return cast(^Frag_Uniforms) &ctx.uniforms[offset]
}

///////////////////////////////////////////////////////////
// CALLBACKS
///////////////////////////////////////////////////////////

__renderFill :: proc(
	uptr: rawptr, 
	paint: ^nvg.Paint, 
	composite_operation: nvg.Composite_Operation_State, 
	scissor: ^scissor,
	fringe: f32,
	bounds: [4]f32,
	paths: []nvg.Path,
) {
	ctx := cast(^Context) uptr
	call := __allocCall(ctx)

	call.type = .Fill
	call.triangle_count = 4
	call.path_offset = __allocPaths(ctx, len(paths))
	call.path_count = len(paths)
	call.image = paint.image
	call.blend_func = __blendCompositeOperation(composite_operation)

	if len(paths) == 1 && paths[0].convex {
		call.type = .Convex_Fill
		call.triangle_count = 0
	}

	// allocate vertices for all the paths
	maxverts := __maxVertCount(paths) + call.triangle_count
	offset := __allocVerts(ctx, maxverts)

	for i in 0..<len(paths) {
		copy := &ctx.paths[call.path_offset + i]
		copy^ = {}
		path := &paths[i]

		if len(path.fill) > 0 {
			copy.fill_offset = offset
			copy.fill_count = len(path.fill)
			mem.copy(&ctx.verts[offset], &path.fill[0], size_of(Vertex) * len(path.fill))
			offset += len(path.fill)
		}

		if len(path.stroke) > 0 {
			copy.stroke_offset = offset
			copy.stroke_count = len(path.stroke)
			mem.copy(&ctx.verts[offset], &path.stroke[0], size_of(Vertex) * len(path.stroke))
			offset += len(path.stroke)
		}
	}

	// setup uniforms for draw calls
	if call.type == .Fill {
		// quad
		call.triangle_offset = offset
		quad := ctx.verts[call.triangle_offset:call.triangle_offset+4]
		quad[0] = { bounds[2], bounds[3], 0.5, 1 }
		quad[1] = { bounds[2], bounds[1], 0.5, 1 }
		quad[2] = { bounds[0], bounds[3], 0.5, 1 }
		quad[3] = { bounds[0], bounds[1], 0.5, 1 }

		// simple shader for stencil
		call.uniform_offset = __allocFragUniforms(ctx, 2)
		frag := __fragUniformPtr(ctx, call.uniform_offset)
		frag^ = {}
		frag.stroke_thr = -1
		frag.type = .Simple

		// fill shader
		__convertPaint(
			ctx, 
			__fragUniformPtr(ctx, call.uniform_offset + ctx.frag_size),
			paint, 
			scissor,
			fringe,
			fringe,
			-1,
		)
	} else {
		call.uniform_offset = __allocFragUniforms(ctx, 1)
		// fill shader
		__convertPaint(
			ctx,
			__fragUniformPtr(ctx, call.uniform_offset),
			paint, 
			scissor,
			fringe,
			fringe,
			-1,
		)
	}
} 

__renderStroke :: proc(
	uptr: rawptr, 
	paint: ^Paint, 
	composite_operation: nvg.Composite_Operation_State, 
	scissor: ^scissor,
	fringe: f32,
	stroke_width: f32,
	paths: []nvg.Path,
) {
	ctx := cast(^Context) uptr
	call := __allocCall(ctx)

	call.type = .Stroke
	call.path_offset = __allocPaths(ctx, len(paths))
	call.path_count = len(paths)
	call.image = paint.image
	call.blend_func = __blendCompositeOperation(composite_operation)

	// allocate vertices for all the paths
	maxverts := __maxVertCount(paths)
	offset := __allocVerts(ctx, maxverts)

	for i in 0..<len(paths) {
		copy := &ctx.paths[call.path_offset + i]
		copy^ = {}
		path := &paths[i]

		if len(path.stroke) != 0 {
			copy.stroke_offset = offset
			copy.stroke_count = len(path.stroke)
			mem.copy(&ctx.verts[offset], &path.stroke[0], size_of(Vertex) * len(path.stroke))
			offset += len(path.stroke)
		}
	}

	if .Stencil_Strokes in ctx.flags {
		// fill shader 
		call.uniform_offset = __allocFragUniforms(ctx, 2)

		__convertPaint(
			ctx,
			__fragUniformPtr(ctx, call.uniform_offset),
			paint,
			scissor,
			stroke_width,
			fringe,
			-1,
		)

		__convertPaint(
			ctx,
			__fragUniformPtr(ctx, call.uniform_offset + ctx.frag_size),
			paint,
			scissor,
			stroke_width,
			fringe,
			1 - 0.5 / 255,
		)
	} else {
		// fill shader
		call.uniform_offset = __allocFragUniforms(ctx, 1)
		__convertPaint(
			ctx,
			__fragUniformPtr(ctx, call.uniform_offset),
			paint,
			scissor,
			stroke_width,
			fringe,
			-1,
		)
	}
}

__renderTriangles :: proc(
	uptr: rawptr, 
	paint: ^Paint, 
	composite_operation: nvg.Composite_Operation_State, 
	scissor: ^scissor,
	verts: []Vertex,
	fringe: f32,
) {
	ctx := cast(^Context) uptr
	call := __allocCall(ctx)

	call.type = .Triangles
	call.image = paint.image
	call.blend_func = __blendCompositeOperation(composite_operation)

	// allocate the vertices for all the paths
	call.triangle_offset = __allocVerts(ctx, len(verts))
	call.triangle_count = len(verts)
	mem.copy(&ctx.verts[call.triangle_offset], raw_data(verts), size_of(Vertex) * len(verts))

	// fill shader
	call.uniform_offset = __allocFragUniforms(ctx, 1)
	frag := __fragUniformPtr(ctx, call.uniform_offset)
	__convertPaint(ctx, frag, paint, scissor, 1, fringe, -1)
	frag.type = .Img	
}

__renderDelete :: proc(uptr: rawptr) {
	ctx := cast(^Context) uptr
	__deleteShader(&ctx.shader)

	when GL3 {
		when GL_USE_UNIFORMBUFFER {
			if ctx.frag_buf != 0 {
				gl.DeleteBuffers(1, &ctx.frag_buf)
			}
		}

		if ctx.vert_arr != 0 {
			gl.DeleteVertexArrays(1, &ctx.vert_arr)
		}
	}

	if ctx.vert_buf != 0 {
		gl.DeleteBuffers(1, &ctx.vert_buf)
	}

	for texture in &ctx.textures {
		if texture.tex != 0 && (.No_Delete not_in texture.flags) {
			gl.DeleteTextures(1, &texture.tex)
		}
	}

	delete(ctx.textures)
	delete(ctx.paths)
	delete(ctx.verts)
	delete(ctx.uniforms)
	delete(ctx.calls)
	free(ctx)
}

///////////////////////////////////////////////////////////
// CREATION?
///////////////////////////////////////////////////////////

Create :: proc(flags: Create_Flags) -> ^nvg.Context {
	ctx := new(Context)
	params: nvg.Params
	params.render_create = __renderCreate
	params.render_create_texture = __renderCreateTexture
	params.render_delete_texture = __renderDeleteTexture
	params.render_update_texture = __renderUpdateTexture
	params.render_get_texture_size = __renderGetTextureSize
	params.render_viewport = __renderViewport
	params.render_cancel = __renderCancel
	params.render_flush = __renderFlush
	params.render_fill = __renderFill
	params.render_stroke = __renderStroke
	params.render_triangles = __renderTriangles
	params.render_delete = __renderDelete
	params.user_ptr = ctx
	params.edge_anti_alias = (.Anti_Alias in flags)
	ctx.flags = flags
	return nvg.CreateInternal(params)
}

Destroy :: proc(ctx: ^nvg.Context) {
	nvg.DeleteInternal(ctx)
}

CreateImageFromHandle :: proc(ctx: ^nvg.Context, textureId: u32, w, h: int, imageFlags: Image_Flags) -> int {
	gctx := cast(^Context) ctx.params.user_ptr
	tex := __allocTexture(gctx)
	tex.type = .RGBA
	tex.tex = textureId
	tex.flags = imageFlags
	tex.width = w
	tex.height = h
	return tex.id
}

ImageHandle :: proc(ctx: ^nvg.Context, textureId: int) -> u32 {
	gctx := cast(^Context) ctx.params.user_ptr
	tex := __findTexture(gctx, textureId)
	return tex.tex
}

// framebuffer additional

framebuffer :: struct {
	ctx: ^nvg.Context,
	fbo: u32,
	rbo: u32,
	texture: u32,
	image: int,
}

DEFAULT_FBO :: 100_000
defaultFBO := i32(DEFAULT_FBO)

// helper function to create GL frame buffer to render to
BindFramebuffer :: proc(fb: ^framebuffer) {
	if defaultFBO == DEFAULT_FBO {
		gl.GetIntegerv(gl.FRAMEBUFFER_BINDING, &defaultFBO)
	}
	gl.BindFramebuffer(gl.FRAMEBUFFER, fb != nil ? fb.fbo : u32(defaultFBO))
}

CreateFramebuffer :: proc(ctx: ^nvg.Context, w, h: int, imageFlags: Image_Flags) -> (fb: framebuffer) {
	defaultFBO: i32
	defaultRBO: i32
	gl.GetIntegerv(gl.FRAMEBUFFER_BINDING, &defaultFBO)
	gl.GetIntegerv(gl.RENDERBUFFER_BINDING, &defaultRBO)

	imageFlags := imageFlags
	incl(&imageFlags, Image_Flags { .Flip_Y, .Premultiplied })
	fb.image = nvg.CreateImageRGBA(ctx, w, h, imageFlags, nil)
	fb.texture = ImageHandle(ctx, fb.image)
	fb.ctx = ctx

	// frame buffer object
	gl.GenFramebuffers(1, &fb.fbo)
	gl.BindFramebuffer(gl.FRAMEBUFFER, fb.fbo)

	// render buffer object
	gl.GenRenderbuffers(1, &fb.rbo)
	gl.BindRenderbuffer(gl.RENDERBUFFER, fb.rbo)
	gl.RenderbufferStorage(gl.RENDERBUFFER, gl.STENCIL_INDEX8, i32(w), i32(h))

	// combine all
	gl.FramebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, fb.texture, 0)
	gl.FramebufferRenderbuffer(gl.FRAMEBUFFER, gl.STENCIL_ATTACHMENT, gl.RENDERBUFFER, fb.rbo)

	if gl.CheckFramebufferStatus(gl.FRAMEBUFFER) != gl.FRAMEBUFFER_COMPLETE {
// #ifdef gl.DEPTH24_STENCIL8
		// If gl.STENCIL_INDEX8 is not supported, try gl.DEPTH24_STENCIL8 as a fallback.
		// Some graphics cards require a depth buffer along with a stencil.
		gl.RenderbufferStorage(gl.RENDERBUFFER, gl.DEPTH24_STENCIL8, i32(w), i32(h))
		gl.FramebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, fb.texture, 0)
		gl.FramebufferRenderbuffer(gl.FRAMEBUFFER, gl.STENCIL_ATTACHMENT, gl.RENDERBUFFER, fb.rbo)

		if gl.CheckFramebufferStatus(gl.FRAMEBUFFER) != gl.FRAMEBUFFER_COMPLETE {
			fmt.eprintln("ERROR")
		}
// #endif // gl.DEPTH24_STENCIL8
// 			goto error
	}

	gl.BindFramebuffer(gl.FRAMEBUFFER, u32(defaultFBO))
	gl.BindRenderbuffer(gl.RENDERBUFFER, u32(defaultRBO))
	return 
}

DeleteFramebuffer :: proc(fb: ^framebuffer) {
	if fb == nil {
		return
	}

	if fb.fbo != 0 {
		gl.DeleteFramebuffers(1, &fb.fbo)
	}
	
	if fb.rbo != 0 {
		gl.DeleteRenderbuffers(1, &fb.rbo)
	}
	
	if fb.image >= 0 {
		nvg.DeleteImage(fb.ctx, fb.image)
	}

	fb.ctx = nil
	fb.fbo = 0
	fb.rbo = 0
	fb.texture = 0
	fb.image = -1
}