package nanovg

// TODO rename structs to old nanovg style!
// TODO rename enums to old nanovg style!

import "core:mem"
import "core:runtime"
import "core:math"
import "core:fmt"
import "../fontstash"
import stbi "vendor:stb/image"

Align_Vertical :: fontstash.Align_Vertical
Align_Horizontal :: fontstash.Align_Horizontal

INIT_FONTIMAGE_SIZE :: 512
MAX_FONTIMAGE_SIZE :: 2048
MAX_FONTIMAGES :: 4

MAX_STATES :: 32
INIT_COMMANDS_SIZE :: 256
INIT_POINTS_SIZE :: 128
INIT_PATH_SIZE :: 16
INIT_VERTS_SIZE :: 26
KAPPA :: 0.5522847493

Color :: [4]f32
Matrix :: [6]f32
// Rect :: [4]f32

Image_Flag :: enum {
	Generate_Mipmaps,
	Repeat_X,
	Repeat_Y,
	Flip_Y,
	Premultiplied,
	Nearest,
	No_Delete,
}
Image_Flags :: bit_set[Image_Flag]

Paint :: struct {
	xform: Matrix,
	extent: [2]f32,
	radius: f32,
	feather: f32,
	inner_color: Color,
	outer_color: Color,
	image: int,
}

Winding :: enum {
	Counter_Clockwise = 1,
	Clockwise,
}

Solidity :: enum {
	Solid = 1, // CCW
	Hole, // CW
}

Line_Cap :: enum {
	Butt,
	Round,
	Square,
	Bevel,
	Miter,
}

Blend_Factor :: enum {
	ZERO,
	ONE,
	SRC_COLOR,
	ONE_MINUS_SRC_COLOR,
	DST_COLOR,
	ONE_MINUS_DST_COLOR,
	SRC_ALPHA,
	ONE_MINUS_SRC_ALPHA,
	DST_ALPHA,
	ONE_MINUS_DST_ALPHA,
	SRC_ALPHA_SATURATE,
}

Composite_Operation :: enum {
	SOURCE_OVER,
	SOURCE_IN,
	SOURCE_OUT,
	ATOP,
	DESTINATION_OVER,
	DESTINATION_IN,
	DESTINATION_OUT,
	DESTINATION_ATOP,
	LIGHTER,
	COPY,
	XOR,
}

Composite_Operation_State :: struct {
	src_RGB: Blend_Factor,
	dst_RGB: Blend_Factor,
	src_alpha: Blend_Factor,
	dst_alpha: Blend_Factor,
}

// render data structures

Vertex :: [4]f32 // x,y,u,v

Texture_Type :: enum {
	Alpha,
	RGBA,
}

scissor :: struct {
	xform: Matrix,
	extent: [2]f32,
}

Command :: enum {
	Move_To,
	Line_To,
	Bezier_To,
	Close,
	Winding,
}

Point_Flag :: enum {
	Corner,
	Left,
	Bevel,
	Inner_Bevel,
}
Point_Flags :: bit_set[Point_Flag]

Point :: struct {
	x, y: f32,
	dx, dy: f32,
	len: f32,
	dmx, dmy: f32,
	flags: Point_Flags,
}

Path_Cache :: struct {
	points: [dynamic]Point,
	paths: [dynamic]Path,
	verts: [dynamic]Vertex,
	bounds: [4]f32,
}

Path :: struct {
	first: int,
	count: int,
	closed: bool,
	nbevel: int,
	fill: []Vertex,
	stroke: []Vertex,
	winding: Winding,
	convex: bool,
}

State :: struct {
	composite_operation: Composite_Operation_State,
	shape_anti_alias: bool,
	fill: Paint,
	stroke: Paint,
	stroke_width: f32,
	miter_limit: f32,
	line_join: Line_Cap,
	line_cap: Line_Cap,
	alpha: f32,
	xform: Matrix,
	scissor: scissor,

	// font state
	font_size: f32,
	letter_spacing: f32,
	line_height: f32,
	font_blur: f32,
	align_horizontal: Align_Horizontal,
	align_vertical: Align_Vertical,
	font_id: int,
}

Context :: struct {
	params: Params,
	commands: [dynamic]f32,
	command_x, command_y: f32,
	states: [MAX_STATES]State,
	state_count: int,
	cache: Path_Cache,
	tess_tol: f32,
	dist_tol: f32,
	fringe_width: f32,
	device_px_ratio: f32,

	// font
	fs: fontstash.Font_Context,
	font_images: [MAX_FONTIMAGES]int,
	font_image_idx: int,

	// stats
	draw_call_count: int,
	fill_tri_count: int,
	stroke_tri_count: int,
	text_tri_count: int,
}

Params :: struct {
	user_ptr: rawptr,
	edge_anti_alias: bool,
	
	// callbacks to fill out
	render_create: proc(uptr: rawptr) -> bool,
	render_delete: proc(uptr: rawptr),

	// textures calls
	render_create_texture: proc(
		uptr: rawptr, 
		type: Texture_Type,
		w, h: int, 
		image_flags: Image_Flags, 
		data: []byte,
	) -> int,
	render_delete_texture: proc(uptr: rawptr, image: int) -> bool,
	render_update_texture: proc(
		uptr: rawptr, 
		image: int,
		x, y: int,
		w, h: int,
		data: []byte,
	) -> bool,
	render_get_texture_size: proc(uptr: rawptr, image: int, w, h: ^int) -> bool,

	// rendering calls
	render_viewport: proc(uptr: rawptr, width, height, device_pixel_ratio: f32),
	render_cancel: proc(uptr: rawptr),
	render_flush: proc(uptr: rawptr),
	render_fill: proc(
		uptr: rawptr, 
		paint: ^Paint, 
		composite_operation: Composite_Operation_State, 
		scissor: ^scissor,
		fringe: f32,
		bounds: [4]f32,
		paths: []Path,
	),
	render_stroke: proc(
		uptr: rawptr, 
		paint: ^Paint, 
		composite_operation: Composite_Operation_State, 
		scissor: ^scissor,
		fringe: f32,
		stroke_width: f32,
		paths: []Path,
	),	
	render_triangles: proc(
		uptr: rawptr, 
		paint: ^Paint, 
		composite_operation: Composite_Operation_State, 
		scissor: ^scissor,
		verts: []Vertex,
		fringe: f32,
	),
}

__allocPathCache :: proc(c: ^Path_Cache) {
	c.points = make([dynamic]Point, 0, INIT_POINTS_SIZE)
	c.paths = make([dynamic]Path, 0, INIT_PATH_SIZE)
	c.verts = make([dynamic]Vertex, 0, INIT_VERTS_SIZE)
}

__deletePathCache :: proc(c: Path_Cache) {
	delete(c.points)
	delete(c.paths)
	delete(c.verts)
}

__setDevicePxRatio :: proc(ctx: ^Context, ratio: f32) {
	ctx.tess_tol = 0.25 / ratio
	ctx.dist_tol = 0.01 / ratio
	ctx.fringe_width = 1.0 / ratio
	ctx.device_px_ratio = ratio
}

__getState :: #force_inline proc(ctx: ^Context) -> ^State #no_bounds_check {
	return &ctx.states[ctx.state_count - 1]
}

CreateInternal :: proc(params: Params) -> (ctx: ^Context) {
	ctx = new(Context)
	ctx.params = params
	ctx.commands = make([dynamic]f32, 0, INIT_COMMANDS_SIZE)
	__allocPathCache(&ctx.cache)

	Save(ctx)
	Reset(ctx)
	__setDevicePxRatio(ctx, 1)

	assert(ctx.params.render_create != nil)
	if !ctx.params.render_create(ctx.params.user_ptr) {
		DeleteInternal(ctx)
		panic("Nanovg - CreateInternal failed")
	}

	w := INIT_FONTIMAGE_SIZE
	h := INIT_FONTIMAGE_SIZE
	fontstash.init(&ctx.fs, w, h)
	assert(ctx.params.render_create_texture != nil)
	ctx.font_images[0] = ctx.params.render_create_texture(ctx.params.user_ptr, .Alpha, w, h, {}, nil)
	ctx.font_image_idx = 0

	return
}

DeleteInternal :: proc(ctx: ^Context) {
	__deletePathCache(ctx.cache)
	fontstash.destroy(&ctx.fs)

	for image in &ctx.font_images {
		if image != 0 {
			DeleteImage(ctx, image)
		}
	}

	if ctx.params.render_delete != nil {
		ctx.params.render_delete(ctx.params.user_ptr)
	}

	free(ctx)
}

/*
	Begin drawing a new frame
	Calls to nanovg drawing API should be wrapped in nvgBeginFrame() & nvgEndFrame()
	nvgBeginFrame() defines the size of the window to render to in relation currently
	set viewport (i.e. glViewport on GL backends). Device pixel ration allows to
	control the rendering on Hi-DPI devices.
	For example, GLFW returns two dimension for an opened window: window size and
	frame buffer size. In that case you would set windowWidth/Height to the window size
	devicePixelRatio to: frameBufferWidth / windowWidth.
*/
BeginFrame :: proc(
	ctx: ^Context,
	window_width: f32,
	window_height: f32,
	device_pixel_ratio: f32,
) {
	ctx.state_count = 0
	Save(ctx)
	Reset(ctx)
	__setDevicePxRatio(ctx, device_pixel_ratio)

	assert(ctx.params.render_viewport != nil)
	ctx.params.render_viewport(ctx.params.user_ptr, window_width, window_height, device_pixel_ratio)

	// TODO render_viewport
	ctx.draw_call_count = 0
	ctx.fill_tri_count = 0
	ctx.stroke_tri_count = 0
	ctx.text_tri_count = 0
}

// Cancels drawing the current frame.
CancelFrame :: proc(ctx: ^Context) {
	assert(ctx.params.render_cancel != nil)
	ctx.params.render_cancel(ctx.params.user_ptr)	
}

// Ends drawing flushing remaining render state.
EndFrame :: proc(ctx: ^Context) {
	assert(ctx.params.render_flush != nil)
	ctx.params.render_flush(ctx.params.user_ptr)	

	// delete textures with invalid size
	if ctx.font_image_idx != 0 {
		font_image := ctx.font_images[ctx.font_image_idx]
		ctx.font_images[ctx.font_image_idx] = 0

		if font_image == 0 {
			return
		}

		iw, ih := ImageSize(ctx, font_image)
		j: int
		for i in 0..<ctx.font_image_idx {
			if ctx.font_images[i] != 0 {
				image := ctx.font_images[i]
				ctx.font_images[i] = 0
				nw, nh := ImageSize(ctx, image)

				if nw < iw || nh < ih {
					DeleteImage(ctx, image)
				} else {
					ctx.font_images[j] = image
					j += 1
				}
			}
		}

		// make current font image to first
		ctx.font_images[j] = ctx.font_images[0]
		ctx.font_images[0] = font_image
		ctx.font_image_idx = 0
	}
}

///////////////////////////////////////////////////////////
// COLORS
//
// Colors in NanoVG are stored as unsigned ints in ABGR format.
///////////////////////////////////////////////////////////

// Returns a color value from red, green, blue values. Alpha will be set to 255 (1.0f).
RGB :: proc(r, g, b: u8) -> Color {
	return RGBA(r, g, b, 255)
}

// Returns a color value from red, green, blue and alpha values.
RGBA :: proc(r, g, b, a: u8) -> (res: Color) {
	res.r = f32(r) / f32(255)
	res.g = f32(g) / f32(255)
	res.b = f32(b) / f32(255)
	res.a = f32(a) / f32(255)
	return
}

// Linearly interpolates from color c0 to c1, and returns resulting color value.
LerpRGBA :: proc(c0, c1: Color, u: f32) -> (cint: Color) {
	u := clamp(u, 0.0, 1.0)
	oneminu := 1.0 - u
	for i in 0..<4 {
		cint[i] = c0[i] * oneminu + c1[i] * u
	}

	return
}

// Returns color value specified by hue, saturation and lightness.
// HSL values are all in range [0..1], alpha will be set to 255.
HSL :: proc(h, s, l: f32) -> Color {
	return HSLA(h,s,l,255)
}

// Returns color value specified by hue, saturation and lightness and alpha.
// HSL values are all in range [0..1], alpha in range [0..255]
HSLA :: proc(h, s, l: f32, a: u8) -> (col: Color) {
	hue :: proc(h, m1, m2: f32) -> f32 {
		h := h

		if h < 0 {
			h += 1
		}
		
		if h > 1 {
			h -= 1
		} 
		
		if h < 1.0 / 6.0 {
			return m1 + (m2 - m1) * h * 6.0
		} else if h < 3.0 / 6.0 {
			return m2
		} else if h < 4.0 / 6.0 {
			return m1 + (m2 - m1) * (2.0 / 3.0 - h) * 6.0
		}

		return m1
	}

	h := math.mod(h, 1.0)
	if h < 0.0 {
		h += 1.0
	} 
	s := clamp(s, 0.0, 1.0)
	l := clamp(l, 0.0, 1.0)
	m2 := l <= 0.5 ? (l * (1 + s)) : (l + s - l * s)
	m1 := 2 * l - m2
	col.r = clamp(hue(h + 1.0/3.0, m1, m2), 0.0, 1.0)
	col.g = clamp(hue(h, m1, m2), 0.0, 1.0)
	col.b = clamp(hue(h - 1.0/3.0, m1, m2), 0.0, 1.0)
	col.a = f32(a) / 255.0
	return
}

///////////////////////////////////////////////////////////
// TRANSFORMS
//
// The following functions can be used to make calculations on 2x3 transformation matrices.
// A 2x3 matrix is represented as float[6].
///////////////////////////////////////////////////////////

// Sets the transform to identity matrix.
TransformIdentity :: proc(t: ^Matrix) {
	t[0] = 1
	t[1] = 0
	t[2] = 0
	t[3] = 1
	t[4] = 0
	t[5] = 0
}

// Sets the transform to translation matrix matrix.
TransformTranslate :: proc(t: ^Matrix, tx, ty: f32) {
	t[0] = 1
	t[1] = 0
	t[2] = 0
	t[3] = 1
	t[4] = tx
	t[5] = ty
}

// Sets the transform to scale matrix.
TransformScale :: proc(t: ^Matrix, sx, sy: f32) {
	t[0] = sx
	t[1] = 0
	t[2] = 0
	t[3] = sy
	t[4] = 0
	t[5] = 0
}

// Sets the transform to rotate matrix. Angle is specified in radians.
TransformRotate :: proc(t: ^Matrix, a: f32) {
	cs := math.cos(a)
	sn := math.sin(a)
	t[0] = cs
	t[1] = sn
	t[2] = -sn
	t[3] = cs
	t[4] = 0
	t[5] = 0
}

// Sets the transform to skew-x matrix. Angle is specified in radians.
TransformSkewX :: proc(t: ^Matrix, a: f32) {
	t[0] = 1
	t[1] = 0
	t[2] = math.tan(a)
	t[3] = 1
	t[4] = 0
	t[5] = 0
}

// Sets the transform to skew-y matrix. Angle is specified in radians.
TransformSkewY :: proc(t: ^Matrix, a: f32) {
	t[0] = 1
	t[1] = math.tan(a)
	t[2] = 0
	t[3] = 1
	t[4] = 0
	t[5] = 0
}

// Sets the transform to the result of multiplication of two transforms, of A = A*B.
TransformMultiply :: proc(t: ^Matrix, s: Matrix) {
	t0 := t[0] * s[0] + t[1] * s[2]
	t2 := t[2] * s[0] + t[3] * s[2]
	t4 := t[4] * s[0] + t[5] * s[2] + s[4]
	t[1] = t[0] * s[1] + t[1] * s[3]
	t[3] = t[2] * s[1] + t[3] * s[3]
	t[5] = t[4] * s[1] + t[5] * s[3] + s[5]
	t[0] = t0
	t[2] = t2
	t[4] = t4
}

// Sets the transform to the result of multiplication of two transforms, of A = B*A.
TransformPremultiply :: proc(t: ^Matrix, s: Matrix) {
	temp := s
	TransformMultiply(&temp, t^)
	t^ = temp
}

// Sets the destination to inverse of specified transform.
// Returns true if the inverse could be calculated, else false.
TransformInverse :: proc(inv: ^Matrix, t: Matrix) -> bool {
	// TODO could be bad math? due to types
	det := f64(t[0]) * f64(t[3]) - f64(t[2]) * f64(t[1])
	
	if det > -1e-6 && det < 1e-6 {
		TransformIdentity(inv)
		return false
	}
	
	invdet := 1.0 / det
	inv[0] = f32(f64(t[3]) * invdet)
	inv[2] = f32(f64(-t[2]) * invdet)
	inv[4] = f32((f64(t[2]) * f64(t[5]) - f64(t[3]) * f64(t[4])) * invdet)
	inv[1] = f32(f64(-t[1]) * invdet)
	inv[3] = f32(f64(t[0]) * invdet)
	inv[5] = f32((f64(t[1]) * f64(t[4]) - f64(t[0]) * f64(t[5])) * invdet)
	return true
}

// Transform a point by given transform.
TransformPoint :: proc(
	dx: ^f32, 
	dy: ^f32, 
	t: Matrix, 
	sx: f32, 
	sy: f32,
) {
	dx^ = sx * t[0] + sy * t[2] + t[4]
	dy^ = sx * t[1] + sy * t[3] + t[5]
}

DegToRad :: proc(deg: f32) -> f32 {
	return deg / 180.0 * math.PI
}

RadToDeg :: proc(rad: f32) -> f32 {
	return rad / math.PI * 180.0
}

///////////////////////////////////////////////////////////
// STATE MANAGEMENT
//
// NanoVG contains state which represents how paths will be rendered.
// The state contains transform, fill and stroke styles, text and font styles,
// and scissor clipping.
///////////////////////////////////////////////////////////

// Pushes and saves the current render state into a state stack.
// A matching nvgRestore() must be used to restore the state.
Save :: proc(ctx: ^Context) {
	if ctx.state_count >= MAX_STATES {
		return
	}

	// copy prior
	if ctx.state_count > 0 {
		ctx.states[ctx.state_count] = ctx.states[ctx.state_count - 1]
	}

	ctx.state_count += 1
}

// Pops and restores current render state.
Restore :: proc(ctx: ^Context) {
	if ctx.state_count <= 1 {
		return
	}

	ctx.state_count -= 1
}

// NOTE useful helper
@(deferred_in=Restore)
SaveScoped :: #force_inline proc(ctx: ^Context) {
	Save(ctx)
}

__setPaintColor :: proc(p: ^Paint, color: Color) {
	p^ = {}
	TransformIdentity(&p.xform)
	p.radius = 0
	p.feather = 1
	p.inner_color = color
	p.outer_color = color
}

// Resets current render state to default values. Does not affect the render state stack.
Reset :: proc(ctx: ^Context) {
	state := __getState(ctx)
	state^ = {}

	__setPaintColor(&state.fill, RGBA(255, 255, 255, 255))
	__setPaintColor(&state.stroke, RGBA(0, 0, 0, 255))

	state.composite_operation = __compositeOperationState(.SOURCE_OVER)
	state.shape_anti_alias = true
	state.stroke_width = 1
	state.miter_limit = 10
	state.line_cap = .Butt
	state.line_join = .Miter
	state.alpha = 1
	TransformIdentity(&state.xform)

	state.scissor.extent[0] = -1
	state.scissor.extent[1] = -1

	// font settings
	state.font_size = 16
	state.letter_spacing = 0
	state.line_height = 1
	state.font_blur = 0
	state.align_horizontal = .Left
	state.align_vertical = .Baseline
	state.font_id = 0
}

///////////////////////////////////////////////////////////
// STATE SETTING
///////////////////////////////////////////////////////////

// Sets whether to draw antialias for nvgStroke() and nvgFill(). It's enabled by default.
ShapeAntiAlias :: proc(ctx: ^Context, enabled: bool) {
	state := __getState(ctx)
	state.shape_anti_alias = enabled
}

// Sets the stroke width of the stroke style.
StrokeWidth :: proc(ctx: ^Context, width: f32) {
	state := __getState(ctx)
	state.stroke_width = width		
}

// Sets the miter limit of the stroke style.
// Miter limit controls when a sharp corner is beveled.
MiterLimit :: proc(ctx: ^Context, limit: f32) {
	state := __getState(ctx)
	state.miter_limit = limit
}

// Sets how the end of the line (cap) is drawn,
// Can be one of: NVG_BUTT (default), NVG_ROUND, NVG_SQUARE.
LineCap :: proc(ctx: ^Context, cap: Line_Cap) {
	state := __getState(ctx)
	state.line_cap = cap
}

// Sets how sharp path corners are drawn.
// Can be one of NVG_MITER (default), NVG_ROUND, NVG_BEVEL.
LineJoin :: proc(ctx: ^Context, join: Line_Cap) {
	state := __getState(ctx)
	state.line_join = join
}

// Sets the transparency applied to all rendered shapes.
// Already transparent paths will get proportionally more transparent as well.
GlobalAlpha :: proc(ctx: ^Context, alpha: f32) {
	state := __getState(ctx)
	state.alpha = alpha
}

// Sets current stroke style to a solid color.
StrokeColor :: proc(ctx: ^Context, color: Color) {
	state := __getState(ctx)
	__setPaintColor(&state.stroke, color)	
}

// Sets current stroke style to a paint, which can be a one of the gradients or a pattern.
StrokePaint :: proc(ctx: ^Context, paint: Paint) {
	state := __getState(ctx)
	state.stroke = paint
	TransformMultiply(&state.stroke.xform, state.xform)
}

// Sets current fill style to a solid color.
FillColor :: proc(ctx: ^Context, color: Color) {
	state := __getState(ctx)
	__setPaintColor(&state.fill, color)	
}

// Sets current fill style to a paint, which can be a one of the gradients or a pattern.
FillPaint :: proc(ctx: ^Context, paint: Paint) {
	state := __getState(ctx)
	state.fill = paint
	TransformMultiply(&state.fill.xform, state.xform)
}

///////////////////////////////////////////////////////////
// STATE TRANSFORMS
//
// The paths, gradients, patterns and scissor region are transformed by an transformation
// matrix at the time when they are passed to the API.
// The current transformation matrix is a affine matrix:
//   [sx kx tx]
//   [ky sy ty]
//   [ 0  0  1]
// Where: sx,sy define scaling, kx,ky skewing, and tx,ty translation.
// The last row is assumed to be 0,0,1 and is not stored.
//
// Apart from nvgResetTransform(), each transformation function first creates
// specific transformation matrix and pre-multiplies the current transformation by it.
//
// Current coordinate system (transformation) can be saved and restored using nvgSave() and nvgRestore().
///////////////////////////////////////////////////////////

Transform :: proc(ctx: ^Context, a, b, c, d, e, f: f32) {
	state := __getState(ctx)
	TransformPremultiply(&state.xform, { a, b, c, d, e, f })	
}

// Resets current transform to a identity matrix.
ResetTransform :: proc(ctx: ^Context) {
	state := __getState(ctx)
	TransformIdentity(&state.xform)
}

// Translates current coordinate system.
Translate :: proc(ctx: ^Context, x, y: f32) {
	state := __getState(ctx)
	temp: Matrix
	TransformTranslate(&temp, x, y)
	TransformPremultiply(&state.xform, temp)
}

// Rotates current coordinate system. Angle is specified in radians.
Rotate :: proc(ctx: ^Context, angle: f32) {
	state := __getState(ctx)
	temp: Matrix
	TransformRotate(&temp, angle)
	TransformPremultiply(&state.xform, temp)
}

// Skews the current coordinate system along X axis. Angle is specified in radians.
SkewX :: proc(ctx: ^Context, angle: f32) {
	state := __getState(ctx)
	temp: Matrix
	TransformSkewX(&temp, angle)
	TransformPremultiply(&state.xform, temp)
}

// Skews the current coordinate system along Y axis. Angle is specified in radians.
SkewY :: proc(ctx: ^Context, angle: f32) {
	state := __getState(ctx)
	temp: Matrix
	TransformSkewY(&temp, angle)
	TransformPremultiply(&state.xform, temp)
}

// Scales the current coordinate system.
Scale :: proc(ctx: ^Context, x, y: f32) {
	state := __getState(ctx)
	temp: Matrix
	TransformScale(&temp, x, y)
	TransformPremultiply(&state.xform, temp)
}

/*
	Stores the top part (a-f) of the current transformation matrix in to the specified buffer.
	  [a c e]
	  [b d f]
	  [0 0 1]
	There should be space for 6 floats in the return buffer for the values a-f.
*/
CurrentTransform :: proc(ctx: ^Context, xform: ^Matrix) {
	state := __getState(ctx)
	if xform == nil {
		return
	}
	xform^ = state.xform
}

///////////////////////////////////////////////////////////
// IMAGE HANDLING
//
// NanoVG allows you to load jpg, png, psd, tga, pic and gif files to be used for rendering.
// In addition you can upload your own image. The image loading is provided by stb_image.
// The parameter imageFlags is a combination of flags defined in NVGimageFlags.
///////////////////////////////////////////////////////////

// Creates image by loading it from the disk from specified file name.
// Returns handle to the image.
CreateImagePath :: proc(ctx: ^Context, filename: cstring, image_flags: Image_Flags) -> int {
	stbi.set_unpremultiply_on_load(1)
	stbi.convert_iphone_png_to_rgb(1)
	w, h, n: i32
	img := stbi.load(filename, &w, &h, &n, 4)
	
	if img == nil {
		return 0
	}

	data := mem.slice_ptr(img, int(w) * int(h) * int(n))
	image := CreateImageRgba(ctx, int(w), int(h), image_flags, data)
	stbi.image_free(img)
	return image
}

// Creates image by loading it from the specified chunk of memory.
// Returns handle to the image.
CreateImageMem :: proc(ctx: ^Context, data: []byte, image_flags: Image_Flags) -> int {
	stbi.set_unpremultiply_on_load(1)
	stbi.convert_iphone_png_to_rgb(1)
	w, h, n: i32
	img := stbi.load_from_memory(raw_data(data), i32(len(data)), &w, &h, &n, 4)
	
	if img == nil {
		return 0
	}

	data := mem.slice_ptr(img, int(w) * int(h) * int(n))
	image := CreateImageRgba(ctx, int(w), int(h), image_flags, data)
	stbi.image_free(img)
	return image
}

CreateImage :: proc { CreateImagePath, CreateImageMem }

// Creates image from specified image data.
// Returns handle to the image.
CreateImageRgba :: proc(ctx: ^Context, w, h: int, image_flags: Image_Flags, data: []byte) -> int {
	assert(ctx.params.render_create_texture != nil)
	return ctx.params.render_create_texture(
		ctx.params.user_ptr,
		.RGBA,
		w, h,
		image_flags,
		data,
	)
}

// Updates image data specified by image handle.
UpdateImage :: proc(ctx: ^Context, image: int, data: []byte) {
	assert(ctx.params.render_get_texture_size != nil)
	assert(ctx.params.render_update_texture != nil)
	
	w, h: int
	found := ctx.params.render_get_texture_size(ctx.params.user_ptr, image, &w, &h)
	if found {
		ctx.params.render_update_texture(ctx.params.user_ptr, image, 0, 0, w, h, data)
	}
}

// Returns the dimensions of a created image.
ImageSize :: proc(ctx: ^Context, image: int) -> (w, h: int) {
	assert(ctx.params.render_get_texture_size != nil)
	ctx.params.render_get_texture_size(ctx.params.user_ptr, image, &w, &h)
	return
}

// Deletes created image.
DeleteImage :: proc(ctx: ^Context, image: int) {
	assert(ctx.params.render_delete_texture != nil)
	ctx.params.render_delete_texture(ctx.params.user_ptr, image)
}

///////////////////////////////////////////////////////////
// PAINT gradients / image
//
// NanoVG supports four types of paints: linear gradient, box gradient, radial gradient and image pattern.
// These can be used as paints for strokes and fills.
///////////////////////////////////////////////////////////

/*
	Creates and returns a linear gradient. Parameters (sx,sy)-(ex,ey) specify the start and end coordinates
	of the linear gradient, icol specifies the start color and ocol the end color.
	The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
*/
LinearGradient :: proc(
	sx, sy: f32,
	ex, ey: f32,
	icol: Color,
	ocol: Color,
) -> (p: Paint) {
	LARGE :: f32(1e5)

	// Calculate transform aligned to the line
	dx := ex - sx
	dy := ey - sy
	d := math.sqrt(dx*dx + dy*dy)
	if d > 0.0001 {
		dx /= d
		dy /= d
	} else {
		dx = 0
		dy = 1
	}

	p.xform[0] = dy
	p.xform[1] = -dx
	p.xform[2] = dx
	p.xform[3] = dy
	p.xform[4] = sx - dx*LARGE
	p.xform[5] = sy - dy*LARGE

	p.extent[0] = LARGE
	p.extent[1] = LARGE + d*0.5

	p.feather = max(1.0, d)

	p.inner_color = icol
	p.outer_color = ocol

	return
}

/*
	Creates and returns a box gradient. Box gradient is a feathered rounded rectangle, it is useful for rendering
	drop shadows or highlights for boxes. Parameters (x,y) define the top-left corner of the rectangle,
	(w,h) define the size of the rectangle, r defines the corner radius, and f feather. Feather defines how blurry
	the border of the rectangle is. Parameter icol specifies the inner color and ocol the outer color of the gradient.
	The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
*/
RadialGradient :: proc(
	cx, cy: f32,
	inr: f32,
	outr: f32,
	icol: Color,
	ocol: Color,
) -> (p: Paint) {
	r := (inr+outr)*0.5
	f := (outr-inr)

	TransformIdentity(&p.xform)
	p.xform[4] = cx
	p.xform[5] = cy

	p.extent[0] = r
	p.extent[1] = r

	p.radius = r
	p.feather = max(1.0, f)

	p.inner_color = icol
	p.outer_color = ocol

	return 
}

/*
	Creates and returns a radial gradient. Parameters (cx,cy) specify the center, inr and outr specify
	the inner and outer radius of the gradient, icol specifies the start color and ocol the end color.
	The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
*/
BoxGradient :: proc(
	x, y: f32,
	w, h: f32,
	r: f32,
	f: f32,
	icol: Color,
	ocol: Color,
) -> (p: Paint) {
	TransformIdentity(&p.xform)
	p.xform[4] = x+w*0.5
	p.xform[5] = y+h*0.5

	p.extent[0] = w*0.5
	p.extent[1] = h*0.5

	p.radius = r
	p.feather = max(1.0, f)

	p.inner_color = icol
	p.outer_color = ocol

	return 
}

/*
	Creates and returns an image pattern. Parameters (ox,oy) specify the left-top location of the image pattern,
	(ex,ey) the size of one image, angle rotation around the top-left corner, image is handle to the image to render.
	The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
*/
ImagePattern :: proc(
	cx, cy: f32,
	w, h: f32,
	angle: f32,
	image: int,
	alpha: f32,
) -> (p: Paint) {
	TransformRotate(&p.xform, angle)
	p.xform[4] = cx
	p.xform[5] = cy

	p.extent[0] = w
	p.extent[1] = h

	p.image = image
	p.inner_color = { 1,1,1,alpha }
	p.outer_color = p.inner_color

	return
}

///////////////////////////////////////////////////////////
// SCISSOR
//
// Scissoring allows you to clip the rendering into a rectangle. This is useful for various
// user interface cases like rendering a text edit or a timeline.
///////////////////////////////////////////////////////////

// Sets the current scissor rectangle.
// The scissor rectangle is transformed by the current transform.
Scissor :: proc(
	ctx: ^Context,
	x, y: f32,
	w, h: f32,
) {
	state := __getState(ctx)
	w := max(w, 0)
	h := max(h, 0)
	
	TransformIdentity(&state.scissor.xform)
	state.scissor.xform[4] = x + w * 0.5
	state.scissor.xform[5] = y + h * 0.5
	TransformMultiply(&state.scissor.xform, state.xform)

	state.scissor.extent[0] = w * 0.5
	state.scissor.extent[1] = h * 0.5
}

/*
	Intersects current scissor rectangle with the specified rectangle.
	The scissor rectangle is transformed by the current transform.
	Note: in case the rotation of previous scissor rect differs from
	the current one, the intersection will be done between the specified
	rectangle and the previous scissor rectangle transformed in the current
	transform space. The resulting shape is always rectangle.
*/
IntersectScissor :: proc(
	ctx: ^Context,
	x, y, w, h: f32,
) {
	isect_rects :: proc(
		dst: ^[4]f32,
		ax, ay, aw, ah: f32,
		bx, by, bw, bh: f32,
	) {
		minx := max(ax, bx)
		miny := max(ay, by)
		maxx := min(ax + aw, bx + bw)
		maxy := min(ay + ah, by + bh)
		dst[0] = minx
		dst[1] = miny
		dst[2] = max(0.0, maxx - minx)
		dst[3] = max(0.0, maxy - miny)
	}

	state := __getState(ctx)
	pxform: Matrix
	invxorm: Matrix

	// If no previous scissor has been set, set the scissor as current scissor.
	if state.scissor.extent[0] < 0 {
		Scissor(ctx, x, y, w, h)
		return
	}

	pxform = state.scissor.xform
	ex := state.scissor.extent[0]
	ey := state.scissor.extent[1]
	TransformInverse(&invxorm, state.xform)
	TransformMultiply(&pxform, invxorm)
	tex := ex * abs(pxform[0]) + ey * abs(pxform[2])
	tey := ex * abs(pxform[1]) + ey * abs(pxform[3])
	
	rect: [4]f32
	isect_rects(&rect, pxform[4] - tex, pxform[5] - tey, tex * 2, tey * 2, x,y,w,h)
	Scissor(ctx, rect.x, rect.y, rect.z, rect.w)
}

// Reset and disables scissoring.
ResetScissor :: proc(ctx: ^Context) {
	state := __getState(ctx)
	state.scissor.xform = 0
	state.scissor.extent[0] = -1
	state.scissor.extent[1] = -1
}

///////////////////////////////////////////////////////////
// Global composite operation
//
// The composite operations in NanoVG are modeled after HTML Canvas API, and
// the blend func is based on OpenGL (see corresponding manuals for more info).
// The colors in the blending state have premultiplied alpha.
///////////////////////////////////////////////////////////

// state table instead of if else chains
OP_STATE_TABLE :: [Composite_Operation][2]Blend_Factor {
	.SOURCE_OVER = { .ONE, .ONE_MINUS_SRC_ALPHA	},
	.SOURCE_IN = { .DST_ALPHA, .ZERO },
	.SOURCE_OUT = { .ONE_MINUS_DST_ALPHA, .ZERO },
	.ATOP = { .DST_ALPHA, .ONE_MINUS_SRC_ALPHA },

	.DESTINATION_OVER = { .ONE_MINUS_DST_ALPHA, .ONE },
	.DESTINATION_IN = { .ZERO, .SRC_ALPHA },
	.DESTINATION_OUT = { .ZERO, .ONE_MINUS_SRC_ALPHA },
	.DESTINATION_ATOP = { .ONE_MINUS_DST_ALPHA, .SRC_ALPHA },

	.LIGHTER = { .ONE, .ONE },
	.COPY = { .ONE, .ZERO },
	.XOR = { .ONE_MINUS_DST_ALPHA, .ONE_MINUS_SRC_ALPHA },
}

__compositeOperationState :: proc(op: Composite_Operation) -> (res: Composite_Operation_State) {
	table := OP_STATE_TABLE
	factors := table[op]
	res.src_RGB = factors.x
	res.dst_RGB = factors.y
	res.src_alpha = factors.x
	res.dst_alpha = factors.y
	return
}

// Sets the composite operation. The op parameter should be one of NVGcompositeOperation.
GlobalCompositeOperation :: proc(ctx: ^Context, op: Composite_Operation) {
	state := __getState(ctx)
	state.composite_operation = __compositeOperationState(op)
}

// Sets the composite operation with custom pixel arithmetic. The parameters should be one of NVGblendFactor.
GlobalCompositeBlendFunc :: proc(ctx: ^Context, sfactor, dfactor: Blend_Factor) {
	GlobalCompositeBlendFuncSeparate(ctx, sfactor, dfactor, sfactor, dfactor)
}

// Sets the composite operation with custom pixel arithmetic for RGB and alpha components separately. The parameters should be one of NVGblendFactor.
GlobalCompositeBlendFuncSeparate :: proc(
	ctx: ^Context,
	src_RGB: Blend_Factor,
	dst_RGB: Blend_Factor,
	src_alpha: Blend_Factor,
	dst_alpha: Blend_Factor,
) {
	op := Composite_Operation_State {
		src_RGB,
		dst_RGB,
		src_alpha,
		dst_alpha,
	}
	state := __getState(ctx)
	state.composite_operation = op
}

///////////////////////////////////////////////////////////
// Points / Path handling
///////////////////////////////////////////////////////////

__cross :: proc(dx0, dy0, dx1, dy1: f32) -> f32 {
	return dx1*dy0 - dx0*dy1
}

__ptEquals :: proc(x1, y1, x2, y2, tol: f32) -> bool {
	dx := x2 - x1
	dy := y2 - y1
	return dx * dx + dy * dy < tol * tol
}

__distPtSeg :: proc(x, y, px, py, qx, qy: f32) -> f32 {
	pqx := qx - px
	pqy := qy - py
	dx := x - px
	dy := y - py
	d := pqx * pqx + pqy * pqy
	t := pqx * dx + pqy * dy
	
	if d > 0 {
		t /= d
	}
	
	if t < 0 {
		t = 0
	} else if t > 1 {
		t = 1
	} 

	dx = px + t * pqx - x
	dy = py + t * pqy - y
	return dx * dx + dy * dy
}

__appendCommands :: proc(ctx: ^Context, values: []f32) {
	state := __getState(ctx)

	if Command(values[0]) != .Close && Command(values[0]) != .Winding {
		ctx.command_x = values[len(values) - 2]
		ctx.command_y = values[len(values) - 1]
	}

	i := 0
	for i < len(values) {
		cmd := Command(values[i])

		switch cmd {
			case .Move_To, .Line_To: {
				TransformPoint(&values[i + 1], &values[i + 2], state.xform, values[i + 1], values[i + 2])
				i += 3
			}

			case .Bezier_To: {
				TransformPoint(&values[i + 1], &values[i + 2], state.xform, values[i + 1], values[i + 2])
				TransformPoint(&values[i + 3], &values[i + 4], state.xform, values[i + 3], values[i + 4])
				TransformPoint(&values[i + 5], &values[i + 6], state.xform, values[i + 5], values[i + 6])
				i += 7
			}

			case .Close: {
				i += 1
			}

			case .Winding: {
				i += 2
			}

			// default
			case: {
				i += 1
			}
		}
	}

	// append values
	append(&ctx.commands, ..values)
}

__clearPathCache :: proc(ctx: ^Context) {
	clear(&ctx.cache.points)
	clear(&ctx.cache.paths)
}

__lastPath :: proc(ctx: ^Context) -> ^Path {
	if len(ctx.cache.paths) > 0 {
		return &ctx.cache.paths[len(ctx.cache.paths) - 1]
	}

	return nil
}

__addPath :: proc(ctx: ^Context) {
	append(&ctx.cache.paths, Path {
		first = len(ctx.cache.points),
		winding = .Counter_Clockwise,
	})
}

__lastPoint :: proc(ctx: ^Context) -> ^Point {
	if len(ctx.cache.paths) > 0 {
		return &ctx.cache.points[len(ctx.cache.points) - 1]
	}

	return nil
}

__addPoint :: proc(ctx: ^Context, x, y: f32, flags: Point_Flags) {
	path := __lastPath(ctx)

	if path == nil {
		return
	}

	if path.count > 0 && len(ctx.cache.points) > 0 {
		pt := __lastPoint(ctx)

		if __ptEquals(pt.x, pt.y, x, y, ctx.dist_tol) {
			pt.flags |= flags
			return
		}
	}

	append(&ctx.cache.points, Point {
		x = x,
		y = y,
		flags = flags,
	})
	path.count += 1
}

__closePath :: proc(ctx: ^Context) {
	path := __lastPath(ctx)
	if path == nil {
		return
	}
	path.closed = true
}

__pathWinding :: proc(ctx: ^Context, winding: Winding) {
	path := __lastPath(ctx)
	if path == nil {
		return
	}
	path.winding = winding
}

__getAverageScale :: proc(t: []f32) -> f32 {
	assert(len(t) > 4)
	sx := math.sqrt(t[0] * t[0] + t[2] * t[2])
	sy := math.sqrt(t[1] * t[1] + t[3] * t[3])
	return (sx + sy) * 0.5
}

__triarea2 :: proc(ax, ay, bx, by, cx, cy: f32) -> f32 {
	abx := bx - ax
	aby := by - ay
	acx := cx - ax
	acy := cy - ay
	return acx * aby - abx * acy
}

__polyArea :: proc(points: []Point) -> f32 {
	area := f32(0)
	
	for i := 2; i < len(points); i += 1 {
		a := &points[0]
		b := &points[i - 1]
		c := &points[i]
		area += __triarea2(a.x, a.y, b.x, b.y, c.x, c.y)
	}
	
	return area * 0.5
}

__polyReverse :: proc(points: []Point) {
	tmp: Point
	i := 0 
	j := len(points) - 1
	
	for i < j {
		tmp = points[i]
		points[i] = points[j]
		points[j] = tmp
		i += 1
		j -= 1
	}
}

__normalize :: proc(x, y: ^f32) -> f32 {
	d := math.sqrt(x^ * x^ + y^ * y^)
	if d > 1e-6 {
		id := 1.0 / d
		x^ *= id
		y^ *= id
	}
	return d
}

__tesselateBezier :: proc(
	ctx: ^Context,
	x1, y1: f32,
	x2, y2: f32,
	x3, y3: f32,
	x4, y4: f32,
	level: int,
	flags: Point_Flags,
) {
	if level > 10 {
		return
	}

	x12 := (x1 + x2) * 0.5
	y12 := (y1 + y2) * 0.5
	x23 := (x2 + x3) * 0.5
	y23 := (y2 + y3) * 0.5
	x34 := (x3 + x4) * 0.5
	y34 := (y3 + y4) * 0.5
	x123 := (x12 + x23) * 0.5
	y123 := (y12 + y23) * 0.5

	dx := x4 - x1
	dy := y4 - y1
	d2 := abs(((x2 - x4) * dy - (y2 - y4) * dx))
	d3 := abs(((x3 - x4) * dy - (y3 - y4) * dx))

	if (d2 + d3)*(d2 + d3) < ctx.tess_tol * (dx*dx + dy*dy) {
		__addPoint(ctx, x4, y4, flags)
		return
	}

	x234 := (x23 + x34) * 0.5
	y234 := (y23 + y34) * 0.5
	x1234 := (x123 + x234) * 0.5
	y1234 := (y123 + y234) * 0.5

	__tesselateBezier(ctx, x1,y1, x12,y12, x123,y123, x1234,y1234, level+1, {})
	__tesselateBezier(ctx, x1234,y1234, x234,y234, x34,y34, x4,y4, level+1, flags)
}

__flattenPaths :: proc(ctx: ^Context) {
	cache := &ctx.cache

	if len(cache.paths) > 0 {
		return
	}

	// flatten
	i := 0
	for i < len(ctx.commands) {
		cmd := Command(ctx.commands[i])
		
		switch cmd {
			case .Move_To: {
				__addPath(ctx)
				p := ctx.commands[i + 1:]
				__addPoint(ctx, p[0], p[1], { .Corner })
				i += 3
			}

			case .Line_To: {
				p := ctx.commands[i + 1:]
				__addPoint(ctx, p[0], p[1], { .Corner })
				i += 3
			}

			case .Bezier_To: {
				last := __lastPoint(ctx)
			
				if last != nil {
					cp1 := ctx.commands[i + 1:]
					cp2 := ctx.commands[i + 3:]
					p := ctx.commands[i + 5:]
					__tesselateBezier(ctx, last.x,last.y, cp1[0],cp1[1], cp2[0],cp2[1], p[0],p[1], 0, { .Corner })
				}

				i += 7
			}

			case .Close: {
				__closePath(ctx)
				i += 1
			}

			case .Winding: {
				__pathWinding(ctx, Winding(ctx.commands[i + 1]))
				i += 2
			}

			case: {
				i += 1
			}
		}
	}

	cache.bounds[0] = 1e6
	cache.bounds[1] = 1e6
	cache.bounds[2] = -1e6
	cache.bounds[3] = -1e6

	// Calculate the direction and length of line segments.
	for j in 0..<len(cache.paths) {
		path := &cache.paths[j]
		pts := cache.points[path.first:]

		// If the first and last points are the same, remove the last, mark as closed path.
		p0 := &pts[path.count-1]
		p1 := &pts[0]
		if __ptEquals(p0.x,p0.y, p1.x,p1.y, ctx.dist_tol) {
			fmt.eprintln("~~~", path.count)
			path.count -= 1
			p0 = &pts[path.count - 1]
			// p0 = mem.ptr_offset(&pts[0], path.count - 1)
			path.closed = true
		}

		// enforce winding
		if path.count > 2 {
			area := __polyArea(pts[:path.count])
			
			if path.winding == .Counter_Clockwise && area < 0 {
				__polyReverse(pts[:path.count])
			}
			
			if path.winding == .Clockwise && area > 0 {
				__polyReverse(pts[:path.count])
			}
		}

		for k in 0..<path.count {
			// Calculate segment direction and length
			p0.dx = p1.x - p0.x
			p0.dy = p1.y - p0.y
			p0.len = __normalize(&p0.dx, &p0.dy)
			
			// Update bounds
			cache.bounds[0] = min(cache.bounds[0], p0.x)
			cache.bounds[1] = min(cache.bounds[1], p0.y)
			cache.bounds[2] = max(cache.bounds[2], p0.x)
			cache.bounds[3] = max(cache.bounds[3], p0.y)
			
			// Advance
			p0 = p1
			p1 = mem.ptr_offset(p1, 1)
		}
	}
}

__curveDivs :: proc(r, arc, tol: f32) -> f32 {
	da := math.acos(r / (r + tol)) * 2
	return max(2, math.ceil(arc / da))
}

__chooseBevel :: proc(
	bevel: bool,
	p0: ^Point,
	p1: ^Point,
	w: f32,
	x0, y0, x1, y1: ^f32,
) {
	if bevel {
		x0^ = p1.x + p0.dy * w
		y0^ = p1.y - p0.dx * w
		x1^ = p1.x + p1.dy * w
		y1^ = p1.y - p1.dx * w
	} else {
		x0^ = p1.x + p1.dmx * w
		y0^ = p1.y + p1.dmy * w
		x1^ = p1.x + p1.dmx * w
		y1^ = p1.y + p1.dmy * w
	}
}

///////////////////////////////////////////////////////////
// Vertice Setting
///////////////////////////////////////////////////////////

// set vertex & increase slice position (decreases length)
__vset :: proc(dst: ^[]Vertex, x, y, u, v: f32, loc := #caller_location) {
	dst[0] = { x, y, u, v }
	dst^ = dst[1:]
}

__roundJoin :: proc(
	dst: ^[]Vertex,
	p0: ^Point,
	p1: ^Point,
	lw: f32,
	rw: f32,
	lu: f32,
	ru: f32,
	ncap: int,
) {
	dlx0 := p0.dy
	dly0 := -p0.dx
	dlx1 := p1.dy
	dly1 := -p1.dx

	if .Left in p1.flags {
		lx0,ly0,lx1,ly1: f32
		__chooseBevel(.Inner_Bevel in p1.flags, p0, p1, lw, &lx0,&ly0, &lx1,&ly1)
		a0 := math.atan2(-dly0, -dlx0)
		a1 := math.atan2(-dly1, -dlx1)
		
		if a1 > a0 {
			a1 -= math.PI * 2
		} 

		__vset(dst, lx0, ly0, lu, 1)
		__vset(dst, p1.x - dlx0 * rw, p1.y - dly0 * rw, ru, 1)

		temp := int(math.ceil((a0 - a1) / math.PI * f32(ncap)))
		n := clamp(temp, 2, ncap)

		for i := 0; i < n; i += 1 {
			u := f32(i) / f32(n - 1)
			a := a0 + u * (a1 - a0)
			rx := p1.x + math.cos(a) * rw
			ry := p1.y + math.sin(a) * rw
			__vset(dst, p1.x, p1.y, 0.5, 1)
			__vset(dst, rx, ry, ru,1)
		}

		__vset(dst, lx1, ly1, lu,1)
		__vset(dst, p1.x - dlx1*rw, p1.y - dly1*rw, ru,1)
	} else {
		rx0,ry0,rx1,ry1: f32
		__chooseBevel(.Inner_Bevel in p1.flags, p0, p1, -rw, &rx0, &ry0, &rx1, &ry1)
		a0 := math.atan2(dly0, dlx0)
		a1 := math.atan2(dly1, dlx1)
		if a1 < a0 {
			a1 += math.PI * 2
		}

		__vset(dst, p1.x + dlx0*rw, p1.y + dly0*rw, lu,1)
		__vset(dst, rx0, ry0, ru,1)

		temp := int(math.ceil((a1 - a0) / math.PI * f32(ncap)))
		n := clamp(temp, 2, ncap)

		for i := 0; i < n; i += 1 {
			u := f32(i) / f32(n - 1)
			a := a0 + u*(a1-a0)
			lx := p1.x + math.cos(a) * lw
			ly := p1.y + math.sin(a) * lw
			__vset(dst, lx, ly, lu, 1)
			__vset(dst, p1.x, p1.y, 0.5, 1)
		}

		__vset(dst, p1.x + dlx1*rw, p1.y + dly1*rw, lu,1)
		__vset(dst, rx1, ry1, ru,1)
	}
}

__bevelJoin :: proc(
	dst: ^[]Vertex,
	p0: ^Point,
	p1: ^Point,
	lw: f32,
	rw: f32,
	lu: f32,
	ru: f32,
) {
	dlx0 := p0.dy
	dly0 := -p0.dx
	dlx1 := p1.dy
	dly1 := -p1.dx

	rx0, ry0, rx1, ry1: f32
	lx0, ly0, lx1, ly1: f32

	if .Left in p1.flags {
		__chooseBevel(.Inner_Bevel in p1.flags, p0, p1, lw, &lx0,&ly0, &lx1,&ly1)

		__vset(dst, lx0, ly0, lu,1)
		__vset(dst, p1.x - dlx0*rw, p1.y - dly0*rw, ru,1)

		if .Bevel in p1.flags {
			__vset(dst, lx0, ly0, lu,1)
			__vset(dst, p1.x - dlx0*rw, p1.y - dly0*rw, ru,1)

			__vset(dst, lx1, ly1, lu,1)
			__vset(dst, p1.x - dlx1*rw, p1.y - dly1*rw, ru,1)
		} else {
			rx0 = p1.x - p1.dmx * rw;
			ry0 = p1.y - p1.dmy * rw;

			__vset(dst, p1.x, p1.y, 0.5,1)
			__vset(dst, p1.x - dlx0*rw, p1.y - dly0*rw, ru,1)

			__vset(dst, rx0, ry0, ru,1)
			__vset(dst, rx0, ry0, ru,1)

			__vset(dst, p1.x, p1.y, 0.5,1)
			__vset(dst, p1.x - dlx1*rw, p1.y - dly1*rw, ru,1)
		}

		__vset(dst, lx1, ly1, lu,1)
		__vset(dst, p1.x - dlx1*rw, p1.y - dly1*rw, ru,1)
	} else {
		__chooseBevel(.Inner_Bevel in p1.flags, p0, p1, -rw, &rx0,&ry0, &rx1,&ry1)

		__vset(dst, p1.x + dlx0*lw, p1.y + dly0*lw, lu,1)
		__vset(dst, rx0, ry0, ru,1)

		if .Bevel in p1.flags {
			__vset(dst, p1.x + dlx0*lw, p1.y + dly0*lw, lu,1)
			__vset(dst, rx0, ry0, ru,1)

			__vset(dst, p1.x + dlx1*lw, p1.y + dly1*lw, lu,1)
			__vset(dst, rx1, ry1, ru,1)
		} else {
			lx0 = p1.x + p1.dmx * lw;
			ly0 = p1.y + p1.dmy * lw;

			__vset(dst, p1.x + dlx0*lw, p1.y + dly0*lw, lu,1)
			__vset(dst, p1.x, p1.y, 0.5,1)

			__vset(dst, lx0, ly0, lu,1)
			__vset(dst, lx0, ly0, lu,1)

			__vset(dst, p1.x + dlx1*lw, p1.y + dly1*lw, lu,1)
			__vset(dst, p1.x, p1.y, 0.5,1)
		}

		__vset(dst, p1.x + dlx1*lw, p1.y + dly1*lw, lu,1)
		__vset(dst, rx1, ry1, ru,1)
	}
}

__buttCapStart :: proc(
	dst: ^[]Vertex,
	p: ^Point,
	dx, dy: f32,
	w: f32,
	d: f32,
	aa: f32,
	u0: f32,
	u1: f32,
) {
	px := p.x - dx * d
	py := p.y - dy * d
	dlx := dy
	dly := -dx
	__vset(dst, px + dlx*w - dx*aa, py + dly*w - dy*aa, u0,0)
	__vset(dst, px - dlx*w - dx*aa, py - dly*w - dy*aa, u1,0)
	__vset(dst, px + dlx*w, py + dly*w, u0,1)
	__vset(dst, px - dlx*w, py - dly*w, u1,1)
}

__buttCapEnd :: proc(
	dst: ^[]Vertex,
	p: ^Point,
	dx, dy: f32,
	w: f32,
	d: f32,
	aa: f32,
	u0: f32,
	u1: f32,
) {
	px := p.x + dx * d
	py := p.y + dy * d
	dlx := dy
	dly := -dx
	__vset(dst, px + dlx*w, py + dly*w, u0,1)
	__vset(dst, px - dlx*w, py - dly*w, u1,1)
	__vset(dst, px + dlx*w + dx*aa, py + dly*w + dy*aa, u0,0)
	__vset(dst, px - dlx*w + dx*aa, py - dly*w + dy*aa, u1,0)
}

__roundCapStart :: proc(
	dst: ^[]Vertex,
	p: ^Point,
	dx, dy: f32,
	w: f32,
	ncap: int,
	u0: f32,
	u1: f32,
) {
	px := p.x
	py := p.y
	dlx := dy
	dly := -dx

	for i in 0..<ncap {
		a := f32(i) / f32(ncap-1) * math.PI
		ax := math.cos(a) * w
		ay := math.sin(a) * w
		__vset(dst, px - dlx*ax - dx*ay, py - dly*ax - dy*ay, u0,1)
		__vset(dst, px, py, 0.5,1)
	}

	__vset(dst, px + dlx*w, py + dly*w, u0,1)
	__vset(dst, px - dlx*w, py - dly*w, u1,1)
}

__roundCapEnd :: proc(
	dst: ^[]Vertex,
	p: ^Point,
	dx, dy: f32,
	w: f32,
	ncap: int,
	u0: f32,
	u1: f32,
) {
	px := p.x
	py := p.y
	dlx := dy
	dly := -dx

	__vset(dst, px + dlx*w, py + dly*w, u0,1)
	__vset(dst, px - dlx*w, py - dly*w, u1,1)
	for i in 0..<ncap {
		a := f32(i) / f32(ncap - 1) * math.PI
		ax := math.cos(a) * w
		ay := math.sin(a) * w
		__vset(dst, px, py, 0.5,1)
		__vset(dst, px - dlx*ax + dx*ay, py - dly*ax + dy*ay, u0,1)
	}
}

__calculateJoins :: proc(
	ctx: ^Context,
	w: f32,
	line_join: Line_Cap,
	miter_limit: f32,
) {
	cache := &ctx.cache
	iw := f32(0)

	if w > 0 {
		iw = 1.0 / w
	} 

	// Calculate which joins needs extra vertices to append, and gather vertex count.
	for path, i in &cache.paths {
		pts := cache.points[path.first:]
		p0 := &pts[path.count-1]
		p1 := &pts[0]
		nleft := 0
		path.nbevel = 0

		for j in 0..<path.count {
			dlx0, dly0, dlx1, dly1, dmr2, __cross, limit: f32
			dlx0 = p0.dy
			dly0 = -p0.dx
			dlx1 = p1.dy
			dly1 = -p1.dx
			// Calculate extrusions
			p1.dmx = (dlx0 + dlx1) * 0.5;
			p1.dmy = (dly0 + dly1) * 0.5;
			dmr2 = p1.dmx*p1.dmx + p1.dmy*p1.dmy;
			if (dmr2 > 0.000001) {
				scale := 1.0 / dmr2;
				if (scale > 600.0) {
					scale = 600.0;
				}
				p1.dmx *= scale;
				p1.dmy *= scale;
			}

			// Clear flags, but keep the corner.
			p1.flags = (.Corner in p1.flags) ? { .Corner } : {}

			// Keep track of left turns.
			__cross = p1.dx * p0.dy - p0.dx * p1.dy;
			if __cross > 0.0 {
				nleft += 1;
				incl(&p1.flags, Point_Flag.Left)
			}

			// Calculate if we should use bevel or miter for inner join.
			limit = max(1.01, min(p0.len, p1.len) * iw);
			if (dmr2 * limit * limit) < 1.0 {
				incl(&p1.flags, Point_Flag.Inner_Bevel)
			}

			// Check to see if the corner needs to be beveled.
			if .Corner in p1.flags {
				if (dmr2 * miter_limit*miter_limit) < 1.0 || line_join == .Bevel || line_join == .Round {
					incl(&p1.flags, Point_Flag.Bevel)
				}
			}

			if (.Bevel in p1.flags) || (.Inner_Bevel in p1.flags) {
				path.nbevel += 1
			}

			p0 = p1
			p1 = mem.ptr_offset(p1, 1)
		}

		path.convex = nleft == path.count;
	}
}

// TODO could be done better? or not need dynamic
__allocTempVerts :: proc(ctx: ^Context, nverts: int) -> []Vertex {
	// old := len(ctx.cache.verts)
	// resize(&ctx.cache.verts, len(ctx.cache.verts) + nverts)
	// return ctx.cache.verts[old:old+nverts]
	resize(&ctx.cache.verts, nverts)
	return ctx.cache.verts[:]
}

__expandStroke :: proc(
	ctx: ^Context,
	w: f32,
	fringe: f32,
	line_cap: Line_Cap,
	line_join: Line_Cap,
	miter_limit: f32,	
) -> bool {
	cache := &ctx.cache
	aa := fringe
	u0 := f32(0.0)
	u1 := f32(1.0)
	ncap := __curveDivs(w, math.PI, ctx.tess_tol)	// Calculate divisions per half circle.

	w := w
	w += aa * 0.5

	// Disable the gradient used for antialiasing when antialiasing is not used.
	if aa == 0.0 {
		u0 = 0.5
		u1 = 0.5
	}

	__calculateJoins(ctx, w, line_join, miter_limit)

	// Calculate max vertex usage.
	cverts := 0
	for path in &cache.paths {
		loop := path.closed
	
		// TODO check if f32 calculation necessary?	
		if line_join == .Round {
			cverts += (path.count + path.nbevel * int(ncap + 2) + 1) * 2 // plus one for loop
		} else {
			cverts += (path.count + path.nbevel*5 + 1) * 2 // plus one for loop
		}

		if !loop {
			// space for caps
			if line_cap == .Round {
				cverts += int(ncap*2 + 2)*2
			} else {
				cverts += (3 + 3)*2
			}
		}
	}

	verts := __allocTempVerts(ctx, cverts)
	dst_index: int

	for i in 0..<len(cache.paths) {
		path := &cache.paths[i]
		pts := cache.points[path.first:]
		p0, p1: ^Point
		start, end: int
		dx, dy: f32

		// nil the fil
		path.fill = nil

		// Calculate fringe or stroke
		loop := path.closed
		dst := verts[dst_index:]
		dst_start_length := len(dst)

		if loop {
			// Looping
			p0 = &pts[path.count-1]
			p1 = &pts[0]
			start = 0
			end = path.count
		} else {
			// Add cap
			p0 = &pts[0]
			p1 = &pts[1]
			start = 1
			end = path.count - 1
		}

		if !loop {
			// Add cap
			dx = p1.x - p0.x;
			dy = p1.y - p0.y;
			__normalize(&dx, &dy);

			if line_cap == .Butt {
				__buttCapStart(&dst, p0, dx, dy, w, -aa*0.5, aa, u0, u1)
			}	else if line_cap == .Butt || line_cap == .Square {
				__buttCapStart(&dst, p0, dx, dy, w, w-aa, aa, u0, u1)
			}	else if line_cap == .Round {
				__roundCapStart(&dst, p0, dx, dy, w, int(ncap), u0, u1)
			}
		}

		for j in start..<end {
			// TODO check this
			// if ((p1.flags & (NVG_PT_BEVEL | NVG_PR_INNERBEVEL)) != 0) {
			if (.Bevel in p1.flags) || (.Inner_Bevel in p1.flags) {
				if line_join == .Round {
					__roundJoin(&dst, p0, p1, w, w, u0, u1, int(ncap))
				} else {
					__bevelJoin(&dst, p0, p1, w, w, u0, u1)
				}
			} else {
				__vset(&dst, p1.x + (p1.dmx * w), p1.y + (p1.dmy * w), u0, 1)
				__vset(&dst, p1.x - (p1.dmx * w), p1.y - (p1.dmy * w), u1, 1)
			}

			p0 = p1 
			p1 = mem.ptr_offset(p1, 1)
		}

		if loop {
			// NOTE use old vertices to loopback!
			// Loop it
			__vset(&dst, verts[dst_index + 0].x, verts[dst_index + 0].y, u0, 1)
			__vset(&dst, verts[dst_index + 1].x, verts[dst_index + 1].y, u1, 1)
		} else {
			// Add cap
			dx = p1.x - p0.x;
			dy = p1.y - p0.y;
			__normalize(&dx, &dy);

			if line_cap == .Butt {
				__buttCapEnd(&dst, p1, dx, dy, w, -aa*0.5, aa, u0, u1)
			}	else if line_cap == .Butt || line_cap == .Square {
				__buttCapEnd(&dst, p1, dx, dy, w, w-aa, aa, u0, u1)
			}	else if line_cap == .Round {
				__roundCapEnd(&dst, p1, dx, dy, w, int(ncap), u0, u1)
			}
		}

		// count of vertices pushed
		dst_diff := dst_start_length - len(dst) 
		// set stroke to the new region
		path.stroke = verts[dst_index:dst_index + dst_diff]
		// move index for next iteration
		dst_index += dst_diff
	}

	return true
}

__expandFill :: proc(
	ctx: ^Context,
	w: f32,
	line_join: Line_Cap,
	miter_limit: f32,
) -> bool {
	cache := &ctx.cache
	aa := ctx.fringe_width
	fringe := w > 0.0
	__calculateJoins(ctx, w, line_join, miter_limit)

	// Calculate max vertex usage.
	cverts := 0
	for path in &cache.paths {
		cverts += path.count + path.nbevel + 1

		if fringe {
			cverts += (path.count + path.nbevel*5 + 1) * 2 // plus one for loop
		}
	}

	convex := len(cache.paths) == 1 && cache.paths[0].convex
	verts := __allocTempVerts(ctx, cverts)
	dst_index: int

	for path in &cache.paths {
		pts := cache.points[path.first:]
		p0, p1: ^Point
		rw, lw, woff: f32
		ru, lu: f32

		// Calculate shape vertices.
		woff = 0.5*aa
		dst := verts[dst_index:]
		dst_start_length := len(dst)

		if fringe {
			// Looping
			p0 = &pts[path.count-1]
			p1 = &pts[0]

			for j in 0..<path.count {
				if .Bevel in p1.flags {
					dlx0 := p0.dy
					dly0 := -p0.dx
					dlx1 := p1.dy
					dly1 := -p1.dx
					
					if .Left in p1.flags {
						lx := p1.x + p1.dmx * woff
						ly := p1.y + p1.dmy * woff
						__vset(&dst, lx, ly, 0.5,1)
					} else {
						lx0 := p1.x + dlx0 * woff
						ly0 := p1.y + dly0 * woff
						lx1 := p1.x + dlx1 * woff
						ly1 := p1.y + dly1 * woff
						__vset(&dst, lx0, ly0, 0.5,1)
						__vset(&dst, lx1, ly1, 0.5,1)
					}
				} else {
					__vset(&dst, p1.x + (p1.dmx * woff), p1.y + (p1.dmy * woff), 0.5,1)
				}

				p0 = p1
				p1 = mem.ptr_offset(p1, 1)
			}
		} else {
			for j in 0..<path.count {
				__vset(&dst, pts[j].x, pts[j].y, 0.5,1)
			}
		}

		dst_diff := dst_start_length - len(dst) 
		path.fill = verts[dst_index:dst_index + dst_diff]

		// advance
		dst_start_length = len(dst)
		dst_index += dst_diff

		// Calculate fringe
		if fringe {
			lw = w + woff
			rw = w - woff
			lu = 0
			ru = 1

			// Create only half a fringe for convex shapes so that
			// the shape can be rendered without stenciling.
			if convex {
				lw = woff	// This should generate the same vertex as fill inset above.
				lu = 0.5	// Set outline fade at middle.
			}

			// Looping
			p0 = &pts[path.count-1]
			p1 = &pts[0]

			for j in 0..<path.count {
				if (.Bevel in p1.flags) || (.Inner_Bevel in p1.flags) {
					__bevelJoin(&dst, p0, p1, lw, rw, lu, ru)
				} else {
					__vset(&dst, p1.x + (p1.dmx * lw), p1.y + (p1.dmy * lw), lu,1)
					__vset(&dst, p1.x - (p1.dmx * rw), p1.y - (p1.dmy * rw), ru,1)
				}

				p0 = p1
				p1 = mem.ptr_offset(p1, 1)
			}

			// Loop it
			__vset(&dst, verts[dst_index + 0].x, verts[dst_index + 0].y, lu,1)
			__vset(&dst, verts[dst_index + 1].x, verts[dst_index + 1].y, ru,1)

			dst_diff := dst_start_length - len(dst) 
			path.stroke = verts[dst_index:dst_index + dst_diff]

			// advance
			dst_index += dst_diff
		} else {
			path.stroke = nil
		}
	}

	return true
}

///////////////////////////////////////////////////////////
// Paths
//
// Drawing a new shape starts with nvgBeginPath(), it clears all the currently defined paths.
// Then you define one or more paths and sub-paths which describe the shape. The are functions
// to draw common shapes like rectangles and circles, and lower level step-by-step functions,
// which allow to define a path curve by curve.
//
// NanoVG uses even-odd fill rule to draw the shapes. Solid shapes should have counter clockwise
// winding and holes should have counter clockwise order. To specify winding of a path you can
// call nvgPathWinding(). This is useful especially for the common shapes, which are drawn CCW.
//
// Finally you can fill the path using current fill style by calling nvgFill(), and stroke it
// with current stroke style by calling nvgStroke().
//
// The curve segments and sub-paths are transformed by the current transform.
///////////////////////////////////////////////////////////

// NOTE: helper to go from Command to f32
__cmdf :: #force_inline proc(cmd: Command) -> f32 {
	return f32(cmd)
}

// Clears the current path and sub-paths.
BeginPath :: proc(ctx: ^Context) {
	clear(&ctx.commands)
	__clearPathCache(ctx)
}

@(deferred_in=Fill)
BeginFill :: proc(ctx: ^Context) {
	BeginPath(ctx)
}

@(deferred_in=Stroke)
BeginStroke :: proc(ctx: ^Context) {
	BeginPath(ctx)
}

@(deferred_in=Stroke)
BeginFillStroke :: proc(ctx: ^Context) {
	BeginPath(ctx)		
}

// Starts new sub-path with specified point as first point.
MoveTo :: proc(ctx: ^Context, x, y: f32) {
	values := [3]f32 { __cmdf(.Move_To), x, y }
	__appendCommands(ctx, values[:])
}

// Adds line segment from the last point in the path to the specified point.
LineTo :: proc(ctx: ^Context, x, y: f32) {
	values := [3]f32 { __cmdf(.Line_To), x, y }
	__appendCommands(ctx, values[:])
}

// Adds cubic bezier segment from last point in the path via two control points to the specified point.
BezierTo :: proc(
	ctx: ^Context, 
	c1x, c1y: f32,
	c2x, c2y: f32,
	x, y: f32,
) {
	values := [?]f32 { __cmdf(.Bezier_To), c1x, c1y, c2x, c2y, x, y }
	__appendCommands(ctx, values[:])
}

// Adds quadratic bezier segment from last point in the path via a control point to the specified point.
QuadTo :: proc(ctx: ^Context, cx, cy, x, y: f32) {
	x0 := ctx.command_x
	y0 := ctx.command_y
	values := [?]f32 {
		__cmdf(.Bezier_To),
		x0 + 2 / 3 * (cx - x0), 
		y0 + 2 / 3 * (cy - y0),
		x + 2 / 3 * (cx - x), 
		y + 2 / 3 * (cy - y),
		x,
		y,
	}
	__appendCommands(ctx, values[:])
}

// Adds an arc segment at the corner defined by the last path point, and two specified points.
ArcTo :: proc(
	ctx: ^Context,
	x1, y1: f32,
	x2, y2: f32,
	radius: f32,
) {
	if len(ctx.commands) == 0 {
		return;
	}

	x0 := ctx.command_x
	y0 := ctx.command_y
	// Handle degenerate cases.
	if __ptEquals(x0,y0, x1,y1, ctx.dist_tol) ||
		__ptEquals(x1,y1, x2,y2, ctx.dist_tol) ||
		__distPtSeg(x1,y1, x0,y0, x2,y2) < ctx.dist_tol*ctx.dist_tol ||
		radius < ctx.dist_tol {
		LineTo(ctx, x1, y1)
		return
	}

	// Calculate tangential circle to lines (x0,y0)-(x1,y1) and (x1,y1)-(x2,y2).
	dx0 := x0-x1
	dy0 := y0-y1
	dx1 := x2-x1
	dy1 := y2-y1
	__normalize(&dx0,&dy0)
	__normalize(&dx1,&dy1)
	a := math.acos(dx0*dx1 + dy0*dy1)
	d := radius / math.tan(a / 2.0)

	if d > 10000 {
		LineTo(ctx, x1, y1)
		return
	}

	a0, a1, cx, cy: f32
	direction: Winding

	if __cross(dx0,dy0, dx1,dy1) > 0.0 {
		cx = x1 + dx0*d + dy0*radius
		cy = y1 + dy0*d + -dx0*radius
		a0 = math.atan2(dx0, -dy0)
		a1 = math.atan2(-dx1, dy1)
		direction = .Clockwise
	} else {
		cx = x1 + dx0*d + -dy0*radius
		cy = y1 + dy0*d + dx0*radius
		a0 = math.atan2(-dx0, dy0)
		a1 = math.atan2(dx1, -dy1)
		direction = .Counter_Clockwise
	}

	Arc(ctx, cx, cy, radius, a0, a1, direction)
}

// Creates new circle arc shaped sub-path. The arc center is at cx,cy, the arc radius is r,
// and the arc is drawn from angle a0 to a1, and swept in direction dir (NVG_CCW, or NVG_CW).
// Angles are specified in radians.
Arc :: proc(ctx: ^Context, cx, cy, r, a0, a1: f32, dir: Winding) {
	move: Command = .Line_To if len(ctx.commands) > 0 else .Move_To

	// Clamp angles
	da := a1 - a0
	if dir == .Clockwise {
		if abs(da) >= math.PI*2 {
			da = math.PI*2
		} else {
			for da < 0.0 {
				da += math.PI*2
			}
		}
	} else {
		if abs(da) >= math.PI*2 {
			da = -math.PI*2
		} else {
			for da > 0.0 {
				da -= math.PI*2
			} 
		}
	}

	// Split arc into max 90 degree segments.
	ndivs := max(1, min((int)(abs(da) / (math.PI*0.5) + 0.5), 5))
	hda := (da / f32(ndivs)) / 2.0
	kappa := abs(4.0 / 3.0 * (1.0 - math.cos(hda)) / math.sin(hda))

	if dir == .Counter_Clockwise {
		kappa = -kappa
	}

	values: [3 + 5 * 7 + 100]f32
	nvals := 0

	px, py, ptanx, ptany: f32
	for i in 0..=ndivs {
		a := a0 + da * f32(i) / f32(ndivs)
		dx := math.cos(a)
		dy := math.sin(a)
		x := cx + dx*r
		y := cy + dy*r
		tanx := -dy*r*kappa
		tany := dx*r*kappa

		if i == 0 {
			values[nvals] = __cmdf(move); nvals += 1
			values[nvals] = x; nvals += 1
			values[nvals] = y; nvals += 1
		} else {
			values[nvals] = __cmdf(.Bezier_To); nvals += 1
			values[nvals] = px + ptanx; nvals += 1
			values[nvals] = py + ptany; nvals += 1
			values[nvals] = x-tanx; nvals += 1
			values[nvals] = y-tany; nvals += 1
			values[nvals] = x; nvals += 1
			values[nvals] = y; nvals += 1
		}
		px = x
		py = y
		ptanx = tanx
		ptany = tany
	}

	// stored internally
	__appendCommands(ctx, values[:nvals])
}

// Closes current sub-path with a line segment.
ClosePath :: proc(ctx: ^Context) {
	values := [1]f32 { __cmdf(.Close) }
	__appendCommands(ctx, values[:])
}

// Sets the current sub-path winding, see NVGwinding and NVGsolidity.
PathWinding :: proc(ctx: ^Context, direction: Winding) {
	values := [2]f32 { __cmdf(.Winding), f32(direction) }
	__appendCommands(ctx, values[:])	
}

// same as path_winding but with different enum
PathSolidity :: proc(ctx: ^Context, solidity: Solidity) {
	values := [2]f32 { __cmdf(.Winding), f32(solidity) }
	__appendCommands(ctx, values[:])	
}

// Creates new rectangle shaped sub-path.
Rect :: proc(ctx: ^Context, x, y, w, h: f32) {
	values := [?]f32 {
		__cmdf(.Move_To), x, y,
		__cmdf(.Line_To), x, y + h,
		__cmdf(.Line_To), x + w, y + h,
		__cmdf(.Line_To), x + w, y,
		__cmdf(.Close),
	}
	__appendCommands(ctx, values[:])
}

// Creates new rounded rectangle shaped sub-path.
RoundedRect :: proc(ctx: ^Context, x, y, w, h, radius: f32) {
	RoundedRectVarying(ctx, x, y, w, h, radius, radius, radius, radius)
}

// Creates new rounded rectangle shaped sub-path with varying radii for each corner.
RoundedRectVarying :: proc(
	ctx: ^Context,
	x, y: f32,
	w, h: f32,
	radius_top_left: f32,
	radius_top_right: f32,
	radius_bottom_right: f32,
	radius_bottom_left: f32,
) {
	if radius_top_left < 0.1 && radius_top_right < 0.1 && radius_bottom_right < 0.1 && radius_bottom_left < 0.1 {
		Rect(ctx, x, y, w, h)
	} else {
		halfw := abs(w) * 0.5
		halfh := abs(h) * 0.5
		rxBL := min(radius_bottom_left, halfw) * math.sign(w)
		ryBL := min(radius_bottom_left, halfh) * math.sign(h)
		rxBR := min(radius_bottom_right, halfw) * math.sign(w)
		ryBR := min(radius_bottom_right, halfh) * math.sign(h)
		rxTR := min(radius_top_right, halfw) * math.sign(w)
		ryTR := min(radius_top_right, halfh) * math.sign(h)
		rxTL := min(radius_top_left, halfw) * math.sign(w)
		ryTL := min(radius_top_left, halfh) * math.sign(h)
		values := [?]f32 {
			__cmdf(.Move_To), x, y + ryTL,
			__cmdf(.Line_To), x, y + h - ryBL,
			__cmdf(.Bezier_To), x, y + h - ryBL*(1 - KAPPA), x + rxBL*(1 - KAPPA), y + h, x + rxBL, y + h,
			__cmdf(.Line_To), x + w - rxBR, y + h,
			__cmdf(.Bezier_To), x + w - rxBR*(1 - KAPPA), y + h, x + w, y + h - ryBR*(1 - KAPPA), x + w, y + h - ryBR,
			__cmdf(.Line_To), x + w, y + ryTR,
			__cmdf(.Bezier_To), x + w, y + ryTR*(1 - KAPPA), x + w - rxTR*(1 - KAPPA), y, x + w - rxTR, y,
			__cmdf(.Line_To), x + rxTL, y,
			__cmdf(.Bezier_To), x + rxTL*(1 - KAPPA), y, x, y + ryTL*(1 - KAPPA), x, y + ryTL,
			__cmdf(.Close),
		}
		__appendCommands(ctx, values[:])
	}
}

// Creates new ellipse shaped sub-path.
Ellipse :: proc(ctx: ^Context, cx, cy, rx, ry: f32) {
	values := [?]f32 {
		__cmdf(.Move_To), cx-rx, cy,
		__cmdf(.Bezier_To), cx-rx, cy+ry*KAPPA, cx-rx*KAPPA, cy+ry, cx, cy+ry,
		__cmdf(.Bezier_To), cx+rx*KAPPA, cy+ry, cx+rx, cy+ry*KAPPA, cx+rx, cy,
		__cmdf(.Bezier_To), cx+rx, cy-ry*KAPPA, cx+rx*KAPPA, cy-ry, cx, cy-ry,
		__cmdf(.Bezier_To), cx-rx*KAPPA, cy-ry, cx-rx, cy-ry*KAPPA, cx-rx, cy,
		__cmdf(.Close)		
	}
	__appendCommands(ctx, values[:])
}

// Creates new circle shaped sub-path.
Circle :: #force_inline proc(ctx: ^Context, cx, cy: f32, radius: f32) {
	Ellipse(ctx, cx, cy, radius, radius)
}

// Fills the current path with current fill style.
Fill :: proc(ctx: ^Context) {
	state := __getState(ctx)
	fill_paint := state.fill

	__flattenPaths(ctx)

	if ctx.params.edge_anti_alias && state.shape_anti_alias {
		__expandFill(ctx, ctx.fringe_width, .Miter, 2.4)
	} else {
		__expandFill(ctx, 0, .Miter, 2.4)
	}

	// apply global alpha
	fill_paint.inner_color.a *= state.alpha
	fill_paint.outer_color.a *= state.alpha

	assert(ctx.params.render_fill != nil)
	ctx.params.render_fill(
		ctx.params.user_ptr,
		&fill_paint,
		state.composite_operation,
		&state.scissor,
		ctx.fringe_width,
		ctx.cache.bounds,
		ctx.cache.paths[:],
	)

	for path in &ctx.cache.paths {
		ctx.fill_tri_count += len(path.fill) - 2
		ctx.fill_tri_count += len(path.stroke) - 2
		ctx.draw_call_count += 2
	}
}

// Fills the current path with current stroke style.
Stroke :: proc(ctx: ^Context) {
	state := __getState(ctx)
	scale := __getAverageScale(state.xform[:])
	stroke_width := clamp(state.stroke_width * scale, 0, 200)
	stroke_paint := state.stroke

	if stroke_width < ctx.fringe_width {
		// If the stroke width is less than pixel size, use alpha to emulate coverage.
		// Since coverage is area, scale by alpha*alpha.
		alpha := clamp(stroke_width / ctx.fringe_width, 0, 1)
		stroke_paint.inner_color.a *= alpha * alpha
		stroke_paint.outer_color.a *= alpha * alpha
		stroke_width = ctx.fringe_width
	}

	// apply global alpha
	stroke_paint.inner_color.a *= state.alpha
	stroke_paint.outer_color.a *= state.alpha

	__flattenPaths(ctx)

	if ctx.params.edge_anti_alias && state.shape_anti_alias {
		__expandStroke(ctx, stroke_width * 0.5, ctx.fringe_width, state.line_cap, state.line_join, state.miter_limit)
	} else {
		__expandStroke(ctx, stroke_width * 0.5, 0, state.line_cap, state.line_join, state.miter_limit)
	}	

	assert(ctx.params.render_stroke != nil)
	ctx.params.render_stroke(
		ctx.params.user_ptr,
		&stroke_paint,
		state.composite_operation,
		&state.scissor,
		ctx.fringe_width,
		stroke_width,
		ctx.cache.paths[:],
	)

	for path in &ctx.cache.paths {
		ctx.stroke_tri_count += len(path.stroke) - 2
		ctx.draw_call_count += 1
	}	
}

DebugDumpPathCache :: proc(ctx: ^Context) {
	fmt.printf("~~~~~~~~~~~~~Dumping %d cached paths\n", len(ctx.cache.paths))
	
	for path, i in &ctx.cache.paths {
		fmt.printf(" - Path %d\n", i)
		
		if len(path.fill) != 0 {
			fmt.printf("   - fill: %d\n", len(path.fill))
			
			for j in 0..<len(path.fill) {
				fmt.printf("%f\t%f\n", path.fill[j].x, path.fill[j].y)
			}
		}

		if len(path.stroke) != 0 {
			fmt.printf("   - stroke: %d\n", len(path.stroke))
			
			for j in 0..<len(path.stroke) {
				fmt.printf("%f\t%f\n", path.stroke[j].x, path.stroke[j].y)
			}
		}
	}
}

///////////////////////////////////////////////////////////
// NanoVG allows you to load .ttf files and use the font to render text.
//
// The appearance of the text can be defined by setting the current text style
// and by specifying the fill color. Common text and font settings such as
// font size, letter spacing and text align are supported. Font blur allows you
// to create simple text effects such as drop shadows.
//
// At render time the font face can be set based on the font handles or name.
//
// Font measure functions return values in local space, the calculations are
// carried in the same resolution as the final rendering. This is done because
// the text glyph positions are snapped to the nearest pixels sharp rendering.
//
// The local space means that values are not rotated or scale as per the current
// transformation. For example if you set font size to 12, which would mean that
// line height is 16, then regardless of the current scaling and rotation, the
// returned line height is always 16. Some measures may vary because of the scaling
// since aforementioned pixel snapping.
//
// While this may sound a little odd, the setup allows you to always render the
// same way regardless of scaling. I.e. following works regardless of scaling:
//
//		const char* txt = "Text me up.";
//		nvgTextBounds(vg, x,y, txt, nil, bounds);
//		nvgBeginPath(vg);
//		nvgRoundedRect(vg, bounds[0],bounds[1], bounds[2]-bounds[0], bounds[3]-bounds[1]);
//		nvgFill(vg);
//
// Note: currently only solid color fill is supported for text.
///////////////////////////////////////////////////////////

CreateFont :: proc(ctx: ^Context, name, filename: string) -> int {
	return fontstash.font_push_file(&ctx.fs, name, filename)
}

CreateFontMem :: proc(ctx: ^Context, name: string, slice: []byte) -> int {
	return fontstash.font_push_slice(&ctx.fs, name, slice)
}

FindFont :: proc(ctx: ^Context, name: string) -> int {
	if name == "" {
		return -1
	}

	return fontstash.font_find_by_name(&ctx.fs, name)
}

FontSize :: proc(ctx: ^Context, size: f32) {
	state := __getState(ctx)
	state.font_size = size
}

FontBlur :: proc(ctx: ^Context, blur: f32) {
	state := __getState(ctx)
	state.font_blur = blur
}

TextLetterSpacing :: proc(ctx: ^Context, spacing: f32) {
	state := __getState(ctx)
	state.letter_spacing = spacing
}

TextLineHeight :: proc(ctx: ^Context, line_height: f32) {
	state := __getState(ctx)
	state.line_height = line_height
}

TextAlignHorizontal :: proc(ctx: ^Context, align: Align_Horizontal) {
	state := __getState(ctx)
	state.align_horizontal = align
}

TextAlignVertical :: proc(ctx: ^Context, align: Align_Vertical) {
	state := __getState(ctx)
	state.align_vertical = align
}

TextAlign :: proc(ctx: ^Context, ah: Align_Horizontal, av: Align_Vertical) {
	state := __getState(ctx)
	state.align_horizontal = ah
	state.align_vertical = av
}

FontFace :: proc(ctx: ^Context, font: string) {
	state := __getState(ctx)
	state.font_id = fontstash.font_find_by_name(&ctx.fs, font)
}

__quantize :: proc(a, d: f32) -> f32 {
	return f32(int(a / d + 0.5)) * d
}

__getFontScale :: proc(state: ^State) -> f32 {
	return min(__quantize(__getAverageScale(state.xform[:]), 0.01), 4.0)
}

__flushTextTexture :: proc(ctx: ^Context) {
	dirty: [4]f32
	assert(ctx.params.render_update_texture != nil)

	if fontstash.validate_texture(&ctx.fs, &dirty) {
		font_image := ctx.font_images[ctx.font_image_idx]
		
		// Update texture
		if font_image != 0 {
			data := ctx.fs.texture_data
			x := dirty[0]
			y := dirty[1]
			w := dirty[2] - dirty[0]
			h := dirty[3] - dirty[1]
			ctx.params.render_update_texture(ctx.params.user_ptr, font_image, int(x), int(y), int(w), int(h), data)
		}
	}
}

__allocTextAtlas :: proc(ctx: ^Context) -> bool {
	__flushTextTexture(ctx)
	
	if ctx.font_image_idx >= MAX_FONTIMAGES - 1 {
		return false
	}
	
	// if next fontImage already have a texture
	iw, ih: int
	if ctx.font_images[ctx.font_image_idx+1] != 0 {
		iw, ih = ImageSize(ctx, ctx.font_images[ctx.font_image_idx+1])
	} else { // calculate the new font image size and create it.
		iw, ih = ImageSize(ctx, ctx.font_images[ctx.font_image_idx])
		
		if iw > ih {
			ih *= 2
		}	else {
			iw *= 2
		}

		if iw > MAX_FONTIMAGE_SIZE || ih > MAX_FONTIMAGE_SIZE {
			iw = MAX_FONTIMAGE_SIZE
			ih = MAX_FONTIMAGE_SIZE
		}

		ctx.font_images[ctx.font_image_idx + 1] = ctx.params.render_create_texture(ctx.params.user_ptr, .Alpha, iw, ih, {}, nil)
	}

	ctx.font_image_idx += 1
	fontstash.reset_atlas(&ctx.fs, iw, ih)

	return true;
}

__renderText :: proc(ctx: ^Context, verts: []Vertex) {
	// disallow 0
	if len(verts) == 0 {
		return
	}

	state := __getState(ctx)
	paint := state.fill

	// Render triangles.
	paint.image = ctx.font_images[ctx.font_image_idx]

	// Apply global alpha
	paint.inner_color.a *= state.alpha
	paint.outer_color.a *= state.alpha

	ctx.params.render_triangles(ctx.params.user_ptr, &paint, state.composite_operation, &state.scissor, verts, ctx.fringe_width)
	
	ctx.draw_call_count += 1
	ctx.text_tri_count += len(verts) / 3
}

__isTransformFlipped :: proc(xform: []f32) -> bool {
	det := xform[0] * xform[3] - xform[2] * xform[1]
	return det < 0
}

Text :: proc(ctx: ^Context, x, y: f32, text: string) -> f32 {
	state := __getState(ctx)
	scale := __getFontScale(state) * ctx.device_px_ratio
	invscale := 1.0 / scale
	is_flipped := __isTransformFlipped(state.xform[:])

	if state.font_id == -1 {
		return x
	}

	fs := &ctx.fs
	fontstash.state_set_size(fs, state.font_size * scale)
	fontstash.state_set_spacing(fs, state.letter_spacing * scale)
	fontstash.state_set_blur(fs, state.font_blur * scale)
	fontstash.state_set_align_horizontal(fs, state.align_horizontal)
	fontstash.state_set_align_vertical(fs, state.align_vertical)
	fontstash.state_set_font(fs, state.font_id)

	cverts := max(2, len(text)) * 6 // conservative estimate.
	verts := __allocTempVerts(ctx, cverts)
	nverts: int

	// TODO add FONS_GLYPH_BITMAP_REQUIRED?
	iter := fontstash.text_iter_init(fs, text, x * scale, y * scale)
	prev_iter := iter
	q: fontstash.Quad
	for fontstash.text_iter_step(&ctx.fs, &iter, &q) {
		c: [4 * 2]f32
		
		if iter.previous_glyph_index == -1 { // can not retrieve glyph?
			if nverts != 0 {
				__renderText(ctx, verts[:])
				nverts = 0
			}

			if !__allocTextAtlas(ctx) {
				break // no memory :(
			}

			iter = prev_iter
			fontstash.text_iter_step(fs, &iter, &q) // try again
			
			if iter.previous_glyph_index == -1 {
				// still can not find glyph?
				break
			} 
		}
		
		prev_iter = iter
		if is_flipped {
			q.y0, q.y1 = q.y1, q.y0
			q.t0, q.t1 = q.t1, q.t0
		}

		// Transform corners.
		TransformPoint(&c[0], &c[1], state.xform, q.x0 * invscale, q.y0 * invscale)
		TransformPoint(&c[2], &c[3], state.xform, q.x1 * invscale, q.y0 * invscale)
		TransformPoint(&c[4], &c[5], state.xform, q.x1 * invscale, q.y1 * invscale)
		TransformPoint(&c[6], &c[7], state.xform, q.x0 * invscale, q.y1 * invscale)
		
		// Create triangles
		if nverts + 6 <= cverts {
			verts[nverts] = { c[0], c[1], q.s0, q.t0 }
			nverts += 1
			verts[nverts] = { c[4], c[5], q.s1, q.t1 }
			nverts += 1
			verts[nverts] = { c[2], c[3], q.s1, q.t0 }
			nverts += 1
			verts[nverts] = { c[0], c[1], q.s0, q.t0 }
			nverts += 1
			verts[nverts] = { c[6], c[7], q.s0, q.t1 }
			nverts += 1
			verts[nverts] = { c[4], c[5], q.s1, q.t1 }
			nverts += 1
		}
	}

	// TODO: add back-end bit to do this just once per frame.
	__flushTextTexture(ctx)
	__renderText(ctx, verts)

	return iter.nextx / scale
}

TextMetrics :: proc(ctx: ^Context) -> (ascender, descender, line_height: f32) {
	state := __getState(ctx)
	scale := __getFontScale(state) * ctx.device_px_ratio
	invscale := 1.0 / scale

	if state.font_id == -1 {
		return
	}

	fs := &ctx.fs
	fontstash.state_set_size(fs, state.font_size*scale)
	fontstash.state_set_spacing(fs, state.letter_spacing*scale)
	fontstash.state_set_blur(fs, state.font_blur*scale)
	fontstash.state_set_align_horizontal(fs, state.align_horizontal)
	fontstash.state_set_align_vertical(fs, state.align_vertical)
	fontstash.state_set_font(fs, state.font_id)

	return fontstash.state_vertical_metrics(fs)
}

TextBounds :: proc(
	ctx: ^Context,
	x, y: f32,
	input: string,
) -> (bounds: [4]f32) {
	state := __getState(ctx)
	scale := __getFontScale(state) * ctx.device_px_ratio
	invscale := 1.0 / scale

	if state.font_id == -1 {
		return {}
	}

	fs := &ctx.fs
	fontstash.state_set_size(fs, state.font_size*scale)
	fontstash.state_set_spacing(fs, state.letter_spacing*scale)
	fontstash.state_set_blur(fs, state.font_blur*scale)
	fontstash.state_set_align_horizontal(fs, state.align_horizontal)
	fontstash.state_set_align_vertical(fs, state.align_vertical)
	fontstash.state_set_font(fs, state.font_id)

	width := fontstash.text_bounds(fs, input, x * scale, y * scale, &bounds)
	
	// Use line bounds for height.
	bounds[1], bounds[3] = fontstash.line_bounds(fs, y*scale)
	bounds[0] *= invscale
	bounds[1] *= invscale
	bounds[2] *= invscale
	bounds[3] *= invscale

	return width * invscale
}

Text_Row :: struct {
	start: int,
	end: int,
	next: int,
	width: f32,
	minx, maxx: f32,
}

Codepoint_Type :: enum {
	Space,
	Newline,
	Char,
	CJK,
}

TextBox :: proc(
	ctx: ^Context, 
	x, y: f32,
	break_row_width: f32,
	input: string,
) {
	state := __getState(ctx)
	rows: [2]Text_Row

	ah := state.align_horizontal
	av := state.align_vertical
	// halign := state.textAlign & (NVG_ALIGN_LEFT | NVG_ALIGN_CENTER | NVG_ALIGN_RIGHT)
	// valign := state.textAlign & (NVG_ALIGN_TOP | NVG_ALIGN_MIDDLE | NVG_ALIGN_BOTTOM | NVG_ALIGN_BASELINE)
	lineh := 0

	if state.font_id == -1 {
		return
	} 

	_, _, line_height := TextMetrics(ctx)
	// state.textAlign = NVG_ALIGN_LEFT | valign
	// TODO wtf are the alignments
	state.align_horizontal = .Left

	y := y
	input_breaks := input
	for {
		nrows := TextBreakLines(ctx, input_breaks, break_row_width, &rows, 2)

		if nrows == 0 {
			break
		}

		for i in 0..<nrows {
			row := &rows[i]
			Text(ctx, x, y, input_breaks[row.start:row.end])		
			y += line_height * state.line_height
		}

		input_breaks = input[rows[nrows - 1].next:]
	}

	state.align_horizontal = ah
	state.align_vertical = av
}

TextBreakLines :: proc(
	ctx: ^Context,
	text: string,
	break_row_width: f32,
	rows: ^[2]Text_Row,
	max_rows: int,
) -> int {
	state := __getState(ctx)
	scale := __getFontScale(state) * ctx.device_px_ratio
	invscale := 1.0 / scale

	row_start_x, row_width, row_min_x, row_max_x: f32

	row_start: int = -1
	row_end: int = -1
	word_start: int = -1
	break_end: int = -1
	word_start_x, word_min_x: f32

	break_width, break_max_x: f32
	type := Codepoint_Type.Space
	ptype := Codepoint_Type.Space
	pcodepoint: rune

	if max_rows == 0 || state.font_id == -1 {
		return 0
	}

	// if (end == nil)
	// 	end = string + strlen(string);

	// if (string == end) return 0;

	fs := &ctx.fs
	fontstash.state_set_size(fs, state.font_size * scale)
	fontstash.state_set_spacing(fs, state.letter_spacing * scale)
	fontstash.state_set_blur(fs, state.font_blur * scale)
	fontstash.state_set_align_horizontal(fs, state.align_horizontal)
	fontstash.state_set_align_vertical(fs, state.align_vertical)
	fontstash.state_set_font(fs, state.font_id)

	break_row_width := break_row_width * scale
	iter := fontstash.text_iter_init(fs, text, 0, 0)
	prev_iter := iter
	q: fontstash.Quad
	nrows: int
	
	for fontstash.text_iter_step(fs, &iter, &q) {
		if iter.previous_glyph_index < 0 && __allocTextAtlas(ctx) { // can not retrieve glyph?
			iter = prev_iter;
			fontstash.text_iter_step(fs, &iter, &q) // try again
		}
		prev_iter = iter

		switch iter.codepoint {
			case 9, 11, 12, 32, 0x00a0: {
				// \t
				// \v
				// \f
				// space
				// NBSP
				type = .Space
			}

			case 10:{
				// \n
				type = pcodepoint == 13 ? .Space : .Newline
			}
			
			case 13: {
				// \r
				type = pcodepoint == 10 ? .Space : .Newline
			}

			case 0x0085: { 
				// NEL
				type = .Newline
			}

			case: {
				if (iter.codepoint >= 0x4E00 && iter.codepoint <= 0x9FFF) ||
					(iter.codepoint >= 0x3000 && iter.codepoint <= 0x30FF) ||
					(iter.codepoint >= 0xFF00 && iter.codepoint <= 0xFFEF) ||
					(iter.codepoint >= 0x1100 && iter.codepoint <= 0x11FF) ||
					(iter.codepoint >= 0x3130 && iter.codepoint <= 0x318F) ||
					(iter.codepoint >= 0xAC00 && iter.codepoint <= 0xD7AF) {
					type = .CJK
				}	else {
					type = .Char
				}
			}
		}

		if type == .Newline {
			// Always handle new lines.
			rows[nrows].start = row_start != -1 ? row_start : iter.str
			rows[nrows].end = row_end != -1 ? row_end : iter.str
			rows[nrows].width = row_width * invscale
			rows[nrows].minx = row_min_x * invscale
			rows[nrows].maxx = row_max_x * invscale
			rows[nrows].next = iter.next
			nrows += 1
			
			if nrows >= max_rows {
				return nrows
			}

			// Set nil break point
			break_end = row_start
			break_width = 0.0
			break_max_x = 0.0
			// Indicate to skip the white space at the beginning of the row.
			row_start = -1
			row_end = -1
			row_width = 0
			row_min_x = 0
			row_max_x = 0
		} else {
			if row_start == -1 {
				// Skip white space until the beginning of the line
				if type == .Char || type == .CJK {
					// The current char is the row so far
					row_start_x = iter.x
					row_start = iter.str
					row_end = iter.next
					row_width = iter.nextx - row_start_x
					row_min_x = q.x0 - row_start_x
					row_max_x = q.x1 - row_start_x
					word_start = iter.str
					word_start_x = iter.x
					word_min_x = q.x0 - row_start_x
					// Set nil break point
					break_end = row_start
					break_width = 0.0
					break_max_x = 0.0
				}
			} else {
				next_width := iter.nextx - row_start_x

				// track last non-white space character
				if type == .Char || type == .CJK {
					row_end = iter.next
					row_width = iter.nextx - row_start_x
					row_max_x = q.x1 - row_start_x
				}
				// track last end of a word
				if ((ptype == .Char || ptype == .CJK) && type == .Space) || type == .CJK {
					break_end = iter.str
					break_width = row_width
					break_max_x = row_max_x
				}
				// track last beginning of a word
				if ((ptype == .Space && (type == .Char || type == .CJK)) || type == .CJK) {
					word_start = iter.str
					word_start_x = iter.x
					word_min_x = q.x0
				}

				// Break to new line when a character is beyond break width.
				if (type == .Char || type == .CJK) && next_width > break_row_width {
					// The run length is too long, need to break to new line.
					if (break_end == row_start) {
						// The current word is longer than the row length, just break it from here.
						rows[nrows].start = row_start
						rows[nrows].end = iter.str
						rows[nrows].width = row_width * invscale
						rows[nrows].minx = row_min_x * invscale
						rows[nrows].maxx = row_max_x * invscale
						rows[nrows].next = iter.str
						nrows += 1

						if nrows >= max_rows {
							return nrows
						}

						row_start_x = iter.x
						row_start = iter.str
						row_end = iter.next
						row_width = iter.nextx - row_start_x
						row_min_x = q.x0 - row_start_x
						row_max_x = q.x1 - row_start_x
						word_start = iter.str
						word_start_x = iter.x
						word_min_x = q.x0 - row_start_x
					} else {
						// Break the line from the end of the last word, and start new line from the beginning of the new.
						rows[nrows].start = row_start
						rows[nrows].end = break_end
						rows[nrows].width = break_width * invscale
						rows[nrows].minx = row_min_x * invscale
						rows[nrows].maxx = break_max_x * invscale
						rows[nrows].next = word_start
						nrows += 1
						if nrows >= max_rows {
							return nrows
						}
						// Update row
						row_start_x = word_start_x
						row_start = word_start
						row_end = iter.next
						row_width = iter.nextx - row_start_x
						row_min_x = word_min_x - row_start_x
						row_max_x = q.x1 - row_start_x
					}
					// Set nil break point
					break_end = row_start
					break_width = 0.0
					break_max_x = 0.0
				}
			}
		}

		pcodepoint = iter.codepoint
		ptype = type
	}

	// Break the line from the end of the last word, and start new line from the beginning of the new.
	if row_start != -1 {
		rows[nrows].start = row_start
		rows[nrows].end = row_end
		rows[nrows].width = row_width * invscale
		rows[nrows].minx = row_min_x * invscale
		rows[nrows].maxx = row_max_x * invscale
		rows[nrows].next = iter.end
		nrows += 1
	}

	return nrows	
}