package fontstash

import "core:runtime"
import "core:fmt"
import "core:log"
import "core:os"
import "core:mem"
import "core:math"
import "core:unicode"
import "core:strings"
import stbtt "vendor:stb/truetype"

// This is a port from Fontstash into odin - specialized for nanovg

// Notable features of Fontstash:
// Contains a *single* channel texture atlas for multiple fonts
// Manages a lookup table for frequent glyphs
// Allows blurred font glyphs
// Atlas can resize

// Changes from the original:
// stb truetype only 
// no scratch allocation -> parts use odins dynamic arrays
// leaves GPU vertex creation & texture management up to the user
// texture atlas expands by default

INVALID :: -1
MAX_STATES :: 20
HASH_LUT_SIZE :: 256
INIT_GLYPHS :: 256
INIT_ATLAS_NODES :: 256
MAX_FALLBACKS :: 20
Glyph_Index :: i32

Align_Horizontal :: enum {
	Left,
	Middle,
	Right,
}

Align_Vertical :: enum {
	Top,
	Middle,
	Bottom,
	Baseline,
}

Font :: struct {
	name: string, // allocated

	info: stbtt.fontinfo,
	loaded_data: []byte,

	ascender: f32,
	descender: f32,
	line_height: f32,

	glyphs: [dynamic]Glyph,
	lut: [HASH_LUT_SIZE]int,

	fallbacks: [MAX_FALLBACKS]int,
	nfallbacks: int,
}

Glyph :: struct {
	codepoint: rune,
	index: Glyph_Index,
	next: int,
	isize: i16,
	blur_size: i16,
	x0, y0, x1, y1: i16,
	xoff, yoff: i16,
	xadvance: i16,
}

Atlas_Node :: struct {
	x, y, width: i16,
}

Vertex :: struct #packed {
	x, y: f32,
	u, v: f32,
	color: [4]u8,
}

Quad_Location :: enum {
	Top_Left,
	Bottom_Left,
}

Font_Context :: struct {
	fonts: [dynamic]Font, // allocated using context.allocator

	// always assuming user wants to resize
	nodes: [dynamic]Atlas_Node,

	// actual pixels
	texture_data: []byte, // allocated using context.allocator
	width, height: int,
	// 1 / texture_atlas_width, 1 / texture_atlas_height
	itw, ith: f32,

	// state 
	states: []State,
	state_count: int, // used states

	location: Quad_Location,

	// dirty rectangle of the texture region that was updated
	dirty_rect: [4]f32,

	// callbacks with user_data passed
	user_data: rawptr, // by default set to the context

	// called when a texture is expanded and needs handling
	callback_resize: proc(data: rawptr, w, h: int), 
	// called in state_end to update the texture region that changed
	callback_update: proc(data: rawptr, dirty_rect: [4]f32, texture_data: rawptr), 
}

Init :: proc(using ctx: ^Font_Context, w, h: int, loc: Quad_Location) {
	user_data = ctx
	location = loc
	fonts = make([dynamic]Font, 0, 8)

	itw = f32(1) / f32(w)
	ith = f32(1) / f32(h)
	texture_data = make([]byte, w * h)
	
	width = w
	height = h
	nodes = make([dynamic]Atlas_Node, 0, INIT_ATLAS_NODES)
	__dirtyRectReset(ctx)

	states = make([]State, MAX_STATES)

	// NOTE NECESSARY
	append(&nodes, Atlas_Node {
		width = i16(w),
	})

	__AtlasAddWhiteRect(ctx, 2, 2)

	PushState(ctx)
	ClearState(ctx)
}

Destroy :: proc(using ctx: ^Font_Context) {
	for font in &fonts {
		delete(font.loaded_data)
		delete(font.glyphs)
	}

	delete(states)
	delete(texture_data)
	delete(fonts)
	delete(nodes)
}

Reset :: proc(using ctx: ^Font_Context) {
	__atlasReset(ctx, width, height)
	__dirtyRectReset(ctx)
	mem.zero_slice(texture_data)

	for font in &fonts {
		__lutReset(&font)
	}

	__AtlasAddWhiteRect(ctx, 2, 2)
	PushState(ctx)
	ClearState(ctx)
}

__atlasInsertNode :: proc(using ctx: ^Font_Context, idx, x, y, w: int) {
	// resize is alright here
	resize(&nodes, len(nodes) + 1)

	// shift nodes up once to leave space at idx
	for i := len(nodes) - 1; i > idx; i -= 1 {
		nodes[i] = nodes[i - 1]
	}

	// set new inserted one to properties
	nodes[idx].x = i16(x)
	nodes[idx].y = i16(y)
	nodes[idx].width = i16(w)
}

__atlasRemoveNode :: proc(using ctx: ^Font_Context, idx: int) {
	if len(nodes) == 0 {
		return
	}

	// remove node at index, shift elements down
	for i in idx..<len(nodes) - 1 {
		nodes[i] = nodes[i + 1]
	}

	// reduce size of array
	raw := transmute(^mem.Raw_Dynamic_Array) &nodes
	raw.len -= 1
}

__atlasExpand :: proc(using ctx: ^Font_Context, w, h: int) {
	if w > width {
		__atlasInsertNode(ctx, len(nodes), width, 0, w - width)
	}

	width = w
	height = h
}

__atlasReset :: proc(using ctx: ^Font_Context, w, h: int) {
	width = w
	height = h
	clear(&nodes)

	// init root node
	append(&nodes, Atlas_Node {
		width = i16(w),
	})
}

__AtlasAddSkylineLevel :: proc(using ctx: ^Font_Context, idx, x, y, w, h: int) {
	// insert new node
	__atlasInsertNode(ctx, idx, x, y + h, w)

	// Delete skyline segments that fall under the shadow of the new segment.
	for i := idx + 1; i < len(nodes); i += 1 {
		if nodes[i].x < nodes[i - 1].x + nodes[i - 1].width {
			shrink := nodes[i-1].x + nodes[i-1].width - nodes[i].x
			nodes[i].x += i16(shrink)
			nodes[i].width -= i16(shrink)
			
			if nodes[i].width <= 0 {
				__atlasRemoveNode(ctx, i)
				i -= 1
			} else {
				break
			}
		} else {
			break
		}
	}

	// Merge same height skyline segments that are next to each other.
	for i := 0; i < len(nodes) - 1; i += 1 {
		if nodes[i].y == nodes[i + 1].y {
			nodes[i].width += nodes[i + 1].width
			__atlasRemoveNode(ctx, i + 1)
			i -= 1
		}
	}
}

__AtlasRectFits :: proc(using ctx: ^Font_Context, i, w, h: int) -> int {
	// Checks if there is enough space at the location of skyline span 'i',
	// and return the max height of all skyline spans under that at that location,
	// (think tetris block being dropped at that position). Or -1 if no space found.
	x := int(nodes[i].x)
	y := int(nodes[i].y)
	
	if x + w > width {
		return -1
	}

	i := i
	space_left := w
	for space_left > 0 {
		if i == len(nodes) {
			return -1
		}

		y = max(y, int(nodes[i].y))
		if y + h > height {
			return -1
		}

		space_left -= int(nodes[i].width)
		i += 1
	}

	return y
}

__AtlasAddRect :: proc(using ctx: ^Font_Context, rw, rh: int) -> (rx, ry: int, ok: bool) {
	besth := height
	bestw := width
	besti, bestx, besty := -1, -1, -1

	// Bottom left fit heuristic.
	for i in 0..<len(nodes) {
		y := __AtlasRectFits(ctx, i, rw, rh)
		
		if y != -1 {
			if y + rh < besth || (y + rh == besth && int(nodes[i].width) < bestw) {
				besti = i
				bestw = int(nodes[i].width)
				besth = y + rh
				bestx = int(nodes[i].x)
				besty = y
			}
		}
	}

	if besti == -1 {
		return
	}

	// Perform the actual packing.
	__AtlasAddSkylineLevel(ctx, besti, bestx, besty, rw, rh) 
	ok = true
	rx = bestx
	ry = besty
	return
}

__AtlasAddWhiteRect :: proc(ctx: ^Font_Context, w, h: int) {
	gx, gy, ok := __AtlasAddRect(ctx, w, h)

	if !ok {
		return
	}

	// Rasterize
	dst := ctx.texture_data[gx + gy * ctx.width:]
	for y in 0..<h {
		for x in 0..<w {
			dst[x] = 0xff
		}

		dst = dst[ctx.width:]
	}

	ctx.dirty_rect[0] = cast(f32) min(int(ctx.dirty_rect[0]), gx)
	ctx.dirty_rect[1] = cast(f32) min(int(ctx.dirty_rect[1]), gy)
	ctx.dirty_rect[2] = cast(f32) max(int(ctx.dirty_rect[2]), gx + w)
	ctx.dirty_rect[3] = cast(f32) max(int(ctx.dirty_rect[3]), gy + h)
}

AddFontPath :: proc(
	ctx: ^Font_Context,
	name: string,
	path: string,
) -> int {
	data, ok := os.read_entire_file(path)

	if !ok {
		log.panicf("FONT: failed to read font at %s", path)
	}

	return AddFontMem(ctx, name, data)
}

// push a font to the font stack
// optionally init with ascii characters at a wanted size
AddFontMem :: proc(
	ctx: ^Font_Context,
	name: string,
	data: []u8, 
) -> int {
	append(&ctx.fonts, Font {})
	res := &ctx.fonts[len(ctx.fonts) - 1]
	res.loaded_data = data
	res.name = strings.clone(name)

	stbtt.InitFont(&res.info, &res.loaded_data[0], 0)
	ascent, descent, line_gap: i32
	stbtt.GetFontVMetrics(&res.info, &ascent, &descent, &line_gap)
	fh := f32(ascent - descent)
	res.ascender = f32(ascent) / fh
	res.descender = f32(descent) / fh
	res.line_height = (fh + f32(line_gap)) / fh
	res.glyphs = make([dynamic]Glyph, 0, INIT_GLYPHS)

	__lutReset(res)
	return len(ctx.fonts) - 1
}

AddFont :: proc { AddFontPath, AddFontMem }

AddFallbackFont :: proc(ctx: ^Font_Context, base, fallback: int) -> bool {
	base_font := __getFont(ctx, base)
	
	if base_font.nfallbacks < MAX_FALLBACKS {
		base_font.fallbacks[base_font.nfallbacks] = fallback
		base_font.nfallbacks += 1
		return true
	}

	return false
}

ResetFallbackFont :: proc(ctx: ^Font_Context, base: int) {
	base_font := __getFont(ctx, base)
	base_font.nfallbacks = 0
	clear(&base_font.glyphs)
	__lutReset(base_font)
}

// find font by name
GetFontByName :: proc(ctx: ^Font_Context, name: string) -> int {
	for font, i in ctx.fonts {
		if font.name == name {
			return i
		}
	}

	return INVALID
}

__lutReset :: proc(font: ^Font) {
	// set lookup table
	for i in 0..<HASH_LUT_SIZE {
		font.lut[i] = -1
	}
}

__hashint :: proc(a: u32) -> u32 {
	a := a
	a += ~(a << 15)
	a ~=  (a >> 10)
	a +=  (a << 3)
	a ~=  (a >> 6)
	a +=  (a << 11)
	a ~=  (a >> 16)
	return a
}

__renderGlyphBitmap :: proc(
	font: ^Font,
	output: []u8,
	out_width: i32,
	out_height: i32,
	out_stride: i32,
	scale_x: f32,
	scale_y: f32,
	glyph_index: Glyph_Index,
) {
	stbtt.MakeGlyphBitmap(&font.info, raw_data(output), out_width, out_height, out_stride, scale_x, scale_y, glyph_index)
}

__buildGlyphBitmap :: proc(
	font: ^Font, 
	glyph_index: Glyph_Index,
	pixel_size: f32,
	scale: f32,
) -> (advance, lsb, x0, y0, x1, y1: i32) {
	stbtt.GetGlyphHMetrics(&font.info, glyph_index, &advance, &lsb)
	stbtt.GetGlyphBitmapBox(&font.info, glyph_index, scale, scale, &x0, &y0, &x1, &y1)
	return
}

// get glyph and push to atlas if not exists
__getGlyph :: proc(
	ctx: ^Font_Context,
	font: ^Font,
	codepoint: rune,
	isize: i16,
	blur_size: i16 = 0,
) -> (res: ^Glyph) #no_bounds_check {
	if isize < 2 {
		return
	}

	// find code point and size
	h := __hashint(u32(codepoint)) & (HASH_LUT_SIZE - 1)
	i := font.lut[h]
	for i != -1 {
		glyph := &font.glyphs[i]
		
		if 
			glyph.codepoint == codepoint && 
			glyph.isize == isize &&
			glyph.blur_size == blur_size 
		{
			res = glyph
			return
		}

		i = glyph.next
	}

	// could not find glyph, create it.
	render_font := font // font used to render
	glyph_index := __getGlyph_index(font, codepoint)
	if glyph_index == 0 {
		// lookout for possible fallbacks
		for i in 0..<font.nfallbacks {
			fallback_font := __getFont(ctx, font.fallbacks[i])
			fallback_index := __getGlyph_index(fallback_font, codepoint)

			if fallback_index != 0 {
				glyph_index = fallback_index
				render_font = fallback_font
				break
			}
		}
	}

	pixel_size := f32(isize) / 10
	blur_size := min(blur_size, 20)
	padding := i16(blur_size + 2) // 2 minimum padding
	scale := __getPixelHeightScale(render_font, pixel_size)
	advance, lsb, x0, y0, x1, y1 := __buildGlyphBitmap(render_font, glyph_index, pixel_size, scale)
	gw := (x1 - x0) + i32(padding) * 2
	gh := (y1 - y0) + i32(padding) * 2 

	// Find free spot for the rect in the atlas
	gx, gy, ok := __AtlasAddRect(ctx, int(gw), int(gh))
	if !ok {
		// try again with expanded
		ExpandAtlas(ctx, ctx.width * 2, ctx.height * 2)
		gx, gy, ok = __AtlasAddRect(ctx, int(gw), int(gh))
	}

	// still not ok?
	if !ok {
		return
	}
	
	// Init glyph.
	append(&font.glyphs, Glyph {
		codepoint = codepoint,
		isize = isize,
		blur_size = blur_size,
		index = glyph_index,
		x0 = i16(gx),
		y0 = i16(gy),
		x1 = i16(i32(gx) + gw),
		y1 = i16(i32(gy) + gh),
		xadvance = i16(scale * f32(advance) * 10),
		xoff = i16(x0 - i32(padding)),
		yoff = i16(y0 - i32(padding)),

		// insert char to hash lookup.
		next = font.lut[h],
	})
	font.lut[h] = len(font.glyphs) - 1
	res = &font.glyphs[len(font.glyphs) - 1]

	// rasterize
	dst := ctx.texture_data[int(res.x0 + padding) + int(res.y0 + padding) * ctx.width:]
	__renderGlyphBitmap(
		render_font,
		dst,
		gw - i32(padding) * 2, 
		gh - i32(padding) * 2, 
		i32(ctx.width), 
		scale,
		scale,
		glyph_index,
	)

	// make sure there is one pixel empty border.
	dst = ctx.texture_data[int(res.x0) + int(res.y0) * ctx.width:]
	// y direction
	for y in 0..<int(gh) {
		dst[y * ctx.width] = 0
		dst[int(gw - 1) + y * ctx.width] = 0
	}
	// x direction
	for x in 0..<int(gw) {
		dst[x] = 0
		dst[x + int(gh - 1) * ctx.width] = 0
	}

	if blur_size > 0 {
		__blur(dst, int(gw), int(gh), ctx.width, blur_size)
	}

	ctx.dirty_rect[0] = cast(f32) min(int(ctx.dirty_rect[0]), int(res.x0))
	ctx.dirty_rect[1] = cast(f32) min(int(ctx.dirty_rect[1]), int(res.y0))
	ctx.dirty_rect[2] = cast(f32) max(int(ctx.dirty_rect[2]), int(res.x1))
	ctx.dirty_rect[3] = cast(f32) max(int(ctx.dirty_rect[3]), int(res.y1))

	return
}

/////////////////////////////////
// blur
/////////////////////////////////

// Based on Exponential blur, Jani Huhtanen, 2006

BLUR_APREC :: 16
BLUR_ZPREC :: 7

__blurCols :: proc(dst: []u8, w, h, dst_stride, alpha: int) {
	dst := dst

	for y in 0..<h {
		z := 0 // force zero border

		for x in 1..<w {
			z += (alpha * ((int(dst[x]) << BLUR_ZPREC) - z)) >> BLUR_APREC
			dst[x] = u8(z >> BLUR_ZPREC)
		}

		dst[w - 1] = 0 // force zero border
		z = 0

		for x := w - 2; x >= 0; x -= 1 {
			z += (alpha * ((int(dst[x]) << BLUR_ZPREC) - z)) >> BLUR_APREC
			dst[x] = u8(z >> BLUR_ZPREC)
		}

		dst[0] = 0 // force zero border
		dst = dst[dst_stride:] // advance slice
	}
}

__blurRows :: proc(dst: []u8, w, h, dst_stride, alpha: int) {
	dst := dst

	for x in 0..<w {
		z := 0 // force zero border
		for y := dst_stride; y < h * dst_stride; y += dst_stride {
			z += (alpha * ((int(dst[y]) << BLUR_ZPREC) - z)) >> BLUR_APREC
			dst[y] = u8(z >> BLUR_ZPREC)
		}

		dst[(h - 1) * dst_stride] = 0 // force zero border
		z = 0

		for y := (h - 2) * dst_stride; y >= 0; y -= dst_stride {
			z += (alpha * ((int(dst[y]) << BLUR_ZPREC) - z)) >> BLUR_APREC
			dst[y] = u8(z >> BLUR_ZPREC)
		}

		dst[0] = 0 // force zero border
		dst = dst[1:] // advance
	}
}

__blur :: proc(dst: []u8, w, h, dst_stride: int, blur_size: i16) {
	assert(blur_size != 0)

	// Calculate the alpha such that 90% of the kernel is within the radius. (Kernel extends to infinity)
	sigma := f32(blur_size) * 0.57735 // 1 / sqrt(3)
	alpha := int((1 << BLUR_APREC) * (1 - math.exp(-2.3 / (sigma + 1))))
	__blurRows(dst, w, h, dst_stride, alpha)
	__blurCols(dst, w, h, dst_stride, alpha)
	__blurRows(dst, w, h, dst_stride, alpha)
	__blurCols(dst, w, h, dst_stride, alpha)
}

/////////////////////////////////
// Texture expansion
/////////////////////////////////

ExpandAtlas :: proc(ctx: ^Font_Context, width, height: int, allocator := context.allocator) -> bool {
	width := max(ctx.width, width)
	height := max(ctx.height, height)

	if width == ctx.width && height == ctx.height {
		return true
	}

	if ctx.callback_resize != nil {
		ctx.callback_resize(ctx.user_data, width, height)
	}

	data := make([]byte, width * height, allocator)

	for i in 0..<ctx.height {
		dst := &data[i * width]
		src := &ctx.texture_data[i * ctx.width]
		mem.copy(dst, src, ctx.width)

		if width > ctx.width {
			mem.set(&data[i * width + ctx.width], 0, width - ctx.width)
		}
	}

	if height > ctx.height {
		mem.set(&data[ctx.height * width], 0, (height - ctx.height) * width)
	}

	delete(ctx.texture_data)
	ctx.texture_data = data

	// increase atlas size
	__atlasExpand(ctx, width, height)

	// add existing data as dirty
	maxy := i16(0)
	for node in ctx.nodes {
		maxy = max(maxy, node.y)
	}
	ctx.dirty_rect[0] = 0
	ctx.dirty_rect[1] = 0
	ctx.dirty_rect[2] = f32(ctx.width)
	ctx.dirty_rect[3] = f32(maxy)

	ctx.width = width
	ctx.height = height
	ctx.itw = 1.0 / f32(width)
	ctx.ith = 1.0 / f32(height)

	return true
}

ResetAtlas :: proc(ctx: ^Font_Context, width, height: int, allocator := context.allocator) -> bool {
	if width == ctx.width && height == ctx.height {
		// just clear
		mem.zero_slice(ctx.texture_data)
	} else {
		// realloc
		ctx.texture_data = make([]byte, width * height, allocator)
	}

	ctx.dirty_rect[0] = f32(width)
	ctx.dirty_rect[1] = f32(height)
	ctx.dirty_rect[2] = 0
	ctx.dirty_rect[3] = 0

	// reset fonts
	for font in &ctx.fonts {
		clear(&font.glyphs)
		__lutReset(&font)
	}

	ctx.width = width
	ctx.height = height
	ctx.itw = 1.0 / f32(width)
	ctx.ith = 1.0 / f32(height)

	__AtlasAddWhiteRect(ctx, 2, 2)
	return true
}

__getGlyph_index :: proc(font: ^Font, codepoint: rune) -> Glyph_Index {
	return stbtt.FindGlyphIndex(&font.info, codepoint)
}

__getPixelHeightScale :: proc(font: ^Font, pixel_height: f32) -> f32 {
	return stbtt.ScaleForPixelHeight(&font.info, pixel_height)
}

__getGlyphKernAdvance :: proc(font: ^Font, glyph1, glyph2: Glyph_Index) -> i32 {
	return stbtt.GetGlyphKernAdvance(&font.info, glyph1, glyph2)
}

// get a font with bounds checking
__getFont :: proc(ctx: ^Font_Context, index: int, loc := #caller_location) -> ^Font #no_bounds_check {
	runtime.bounds_check_error_loc(loc, index, len(ctx.fonts))
	return &ctx.fonts[index]
}

// only useful for single glyphs where you quickly want the width
CodepointWidth :: proc(
	font: ^Font,
	codepoint: rune,
	scale: f32,
) -> f32 {
	glyph_index := __getGlyph_index(font, codepoint)
	xadvance, lsb: i32
	stbtt.GetGlyphHMetrics(&font.info, glyph_index, &xadvance, &lsb)
	return f32(xadvance) * scale
}

// get top and bottom line boundary
LineBounds :: proc(ctx: ^Font_Context, y: f32) -> (miny, maxy: f32) {
	state := __getState(ctx)
	font := __getFont(ctx, state.font)
	isize := i16(state.size * 10.0)
	y := y
	y += __getVerticalAlign(ctx, font, state.av, isize)

	if ctx.location == .Top_Left {
		miny = y - font.ascender * f32(isize) / 10
		maxy = miny + font.line_height * f32(isize / 10)
	} else if ctx.location == .Bottom_Left {
		miny = y + font.ascender * f32(isize) / 10
		maxy = miny - font.line_height * f32(isize / 10)
	}

	return
}

// reset dirty rect
__dirtyRectReset :: proc(using ctx: ^Font_Context) {
	dirty_rect[0] = f32(width)
	dirty_rect[1] = f32(height)
	dirty_rect[2] = 0
	dirty_rect[3] = 0
}

// true when the dirty rectangle is valid and needs a texture update on the gpu
ValidateTexture :: proc(using ctx: ^Font_Context, dirty: ^[4]f32) -> bool {
	if dirty_rect[0] < dirty_rect[2] && dirty_rect[1] < dirty_rect[3] {
		dirty[0] = dirty_rect[0]
		dirty[1] = dirty_rect[1]
		dirty[2] = dirty_rect[2]
		dirty[3] = dirty_rect[3]
		__dirtyRectReset(ctx)
		return true
	}

	return false
}

// get alignment based on font
__getVerticalAlign :: proc(
	ctx: ^Font_Context,
	font: ^Font,
	av: Align_Vertical,
	pixel_size: i16,
) -> (res: f32) {
	switch ctx.location {
		case .Top_Left: {
			switch av {
				case .Top: res = font.ascender * f32(pixel_size) / 10
				case .Middle: res = (font.ascender + font.descender) / 2 * f32(pixel_size) / 10
				case .Baseline: res = 0
				case .Bottom: res = font.descender * f32(pixel_size) / 10
			}
		}

		case .Bottom_Left: {
			switch av {
				case .Top: res = -font.ascender * f32(pixel_size) / 10
				case .Middle: res = -(font.ascender + font.descender) / 2 * f32(pixel_size) / 10
				case .Baseline: res = 0
				case .Bottom: res = -font.descender * f32(pixel_size) / 10
			}
		}
	}

	return
}

@(private)
UTF8_ACCEPT :: 0

@(private)
UTF8_REJECT :: 1

@(private)
utf8d := [400]u8 {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 00..1f
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 20..3f
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 40..5f
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 60..7f
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, // 80..9f
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, // a0..bf
	8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, // c0..df
	0xa,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x4,0x3,0x3, // e0..ef
	0xb,0x6,0x6,0x6,0x5,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8, // f0..ff
	0x0,0x1,0x2,0x3,0x5,0x8,0x7,0x1,0x1,0x1,0x4,0x6,0x1,0x1,0x1,0x1, // s0..s0
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1, // s1..s2
	1,2,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1, // s3..s4
	1,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,3,1,1,1,1,1,1, // s5..s6
	1,3,1,1,1,1,1,3,1,3,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // s7..s8
}

// decode codepoints from a state
@(private)
__decutf8 :: #force_inline proc(state: ^rune, codep: ^rune, b: byte) -> bool {
	b := rune(b)
	type := utf8d[b]
	codep^ = (state^ != UTF8_ACCEPT) ? ((b & 0x3f) | (codep^ << 6)) : ((0xff >> type) & (b))
	state^ = rune(utf8d[256 + state^ * 16 + rune(type)])
	return state^ == UTF8_ACCEPT
}

// state used to share font options
State :: struct {
	font: int,
	size: f32,
	color: [4]u8,
	spacing: f32,
	blur: f32,

	ah: Align_Horizontal,
	av: Align_Vertical,
}

// quad that should be used to draw from the texture atlas
Quad :: struct {
	x0, y0, s0, t0: f32,
	x1, y1, s1, t1: f32,
}

// text iteration with custom settings
Text_Iter :: struct {
	x, y, nextx, nexty, scale, spacing: f32,
	isize, iblur: i16,

	font: ^Font,
	previous_glyph_index: Glyph_Index,

	// unicode iteration
	utf8state: rune, // utf8
	codepoint: rune,
	text: string,
	codepoint_count: int,

	// byte indices
	str: int,
	next: int,
	end: int,
}

// push a state, copies the current one over to the next one
PushState :: proc(using ctx: ^Font_Context, loc := #caller_location) #no_bounds_check {
	runtime.bounds_check_error_loc(loc, state_count, MAX_STATES)

	if state_count > 0 {
		states[state_count] = states[state_count - 1]
	}

	state_count += 1
}

// pop a state 
PopState :: proc(using ctx: ^Font_Context) {
	if state_count <= 1 {
		log.error("FONTSTASH: state underflow! to many pops were called")
	} else {
		state_count -= 1
	}
}

// clear current state
ClearState :: proc(ctx: ^Font_Context) {
	state := __getState(ctx)
	state.size = 12
	state.color = 255
	state.blur = 0
	state.spacing = 0
	state.font = 0
	state.ah = .Left
	state.av = .Baseline
}

__getState :: #force_inline proc(ctx: ^Font_Context) -> ^State #no_bounds_check {
	return &ctx.states[ctx.state_count - 1]
}

SetSize :: proc(ctx: ^Font_Context, size: f32) {
	__getState(ctx).size = size
}

SetColor :: proc(ctx: ^Font_Context, color: [4]u8) {
	__getState(ctx).color = color
}

SetSpacing :: proc(ctx: ^Font_Context, spacing: f32) {
	__getState(ctx).spacing = spacing
}

SetBlur :: proc(ctx: ^Font_Context, blur: f32) {
	__getState(ctx).blur = blur
}

SetFont :: proc(ctx: ^Font_Context, font: int) {
	__getState(ctx).font = font
}

SetAh :: SetAlignHorizontal
SetAV :: SetAlignVertical

SetAlignHorizontal :: proc(ctx: ^Font_Context, ah: Align_Horizontal) {
	__getState(ctx).ah = ah
}

SetAlignVertical :: proc(ctx: ^Font_Context, av: Align_Vertical) {
	__getState(ctx).av = av
}

__getQuad :: proc(
	ctx: ^Font_Context,
	font: ^Font,
	
	previous_glyph_index: i32,
	glyph: ^Glyph,

	scale: f32,
	spacing: f32,
	
	x, y: ^f32,
	quad: ^Quad,
) {
	if previous_glyph_index != -1 {
		adv := f32(__getGlyphKernAdvance(font, previous_glyph_index, glyph.index)) * scale
		x^ += f32(int(adv + spacing + 0.5))
	}

	// fill props right
	rx, ry, x0, y0, x1, y1, xoff, yoff, glyph_width, glyph_height: f32
	xoff = f32(glyph.xoff + 1)
	yoff = f32(glyph.yoff + 1)
	x0 = f32(glyph.x0 + 1)
	y0 = f32(glyph.y0 + 1)
	x1 = f32(glyph.x1 - 1)
	y1 = f32(glyph.y1 - 1)

	switch ctx.location {
		case .Top_Left: {
			rx = math.floor(x^ + xoff)
			ry = math.floor(y^ + yoff)
			
			quad.x0 = rx
			quad.y0 = ry
			quad.x1 = rx + x1 - x0
			quad.y1 = ry + y1 - y0

			quad.s0 = x0 * ctx.itw
			quad.t0 = y0 * ctx.ith
			quad.s1 = x1 * ctx.itw
			quad.t1 = y1 * ctx.ith
		}

		case .Bottom_Left: {
			rx = math.floor(x^ + xoff)
			ry = math.floor(y^ - yoff)

			quad.x0 = rx
			quad.y0 = ry
			quad.x1 = rx + x1 - x0
			quad.y1 = ry - y1 + y0

			quad.s0 = x0 * ctx.itw
			quad.t0 = y0 * ctx.ith
			quad.s1 = x1 * ctx.itw
			quad.t1 = y1 * ctx.ith
		}
	}

	x^ += f32(int(f32(glyph.xadvance) / 10 + 0.5))
}

// init text iter struct with settings
TextIterInit :: proc(
	ctx: ^Font_Context,
	x: f32,
	y: f32,
	text: string,
) -> (res: Text_Iter) {
	state := __getState(ctx)
	res.font = __getFont(ctx, state.font)
	res.isize = i16(f32(state.size) * 10)
	res.iblur = i16(state.blur)
	res.scale = __getPixelHeightScale(res.font, f32(res.isize) / 10)

	// align horizontally
	x := x
	y := y
	switch state.ah {
		case .Left: {}
		case .Middle: {
			width := TextBounds(ctx, text, x, y, nil)
			x = math.round(x - width * 0.5)
		}
		case .Right: {
			width := TextBounds(ctx, text, x, y, nil)
			x -= width
		}
	}

	// align vertically
	y = math.round(y + __getVerticalAlign(ctx, res.font, state.av, res.isize))

	// set positions
	res.x = x
	res.nextx = x
	res.y = y
	res.nexty = y
	res.previous_glyph_index = -1
	res.spacing = state.spacing
	res.text = text

	res.str = 0
	res.next = 0
	res.end = len(text)

	return
}

// step through each codepoint
TextIterNext :: proc(
	ctx: ^Font_Context, 
	iter: ^Text_Iter, 
	quad: ^Quad,
) -> (ok: bool) {
	str := iter.next
	iter.str = iter.next

	for str < iter.end {
		defer str += 1

		if __decutf8(&iter.utf8state, &iter.codepoint, iter.text[str]) {
			iter.x = iter.nextx
			iter.y = iter.nexty
			iter.codepoint_count += 1
			glyph := __getGlyph(ctx, iter.font, iter.codepoint, iter.isize, iter.iblur)
			
			if glyph != nil {
				__getQuad(ctx, iter.font, iter.previous_glyph_index, glyph, iter.scale, iter.spacing, &iter.nextx, &iter.nexty, quad)
			}

			iter.previous_glyph_index = glyph == nil ? -1 : glyph.index
			ok = true
			break
		}
	}

	iter.next = str
	return
}

// width of a text line, optionally the full rect
TextBounds :: proc(
	ctx: ^Font_Context,
	text: string,
	x: f32 = 0,
	y: f32 = 0,
	bounds: ^[4]f32 = nil,
) -> f32 {
	state := __getState(ctx)
	isize := i16(state.size * 10)
	iblur := i16(state.blur)
	font := __getFont(ctx, state.font)

	// bunch of state
	x := x
	y := y
	minx := x
	maxx := x
	miny := y 
	maxy := y
	start_x := x

	// iterate	
	scale := __getPixelHeightScale(font, f32(isize) / 10)
	previous_glyph_index: Glyph_Index = -1
	quad: Quad
	utf8state: rune
	codepoint: rune
	for byte_offset in 0..<len(text) {
		if __decutf8(&utf8state, &codepoint, text[byte_offset]) {
			glyph := __getGlyph(ctx, font, codepoint, isize, iblur)

			if glyph != nil {
				__getQuad(ctx, font, previous_glyph_index, glyph, scale, state.spacing, &x, &y, &quad)

				if quad.x0 < minx {
					minx = quad.x0
				}
				if quad.x1 > maxx {
					maxx = quad.x1
				}

				if ctx.location == .Top_Left {
					if quad.y0 < miny {
						miny = quad.y0
					}
					if quad.y1 > maxy {
						maxy = quad.y1
					}
				} else if ctx.location == .Bottom_Left {
					if quad.y1 < miny {
						miny = quad.y1
					}
					if quad.y0 > maxy {
						maxy = quad.y0
					}
				}
			}

			previous_glyph_index = glyph == nil ? -1 : glyph.index
		}
	}

	// horizontal alignment
	advance := x - start_x
	switch state.ah {
		case .Left: {}
		case .Middle: {
			minx -= advance * 0.5
			maxx -= advance * 0.5
		}
		case .Right: {
			minx -= advance
			maxx -= advance
		}
	}

	if bounds != nil {
		bounds^ = { minx, miny, maxx, maxy }
	}

	return advance
}

VerticalMetrics :: proc(
	ctx: ^Font_Context,
) -> (ascender, descender, line_height: f32) {
	state := __getState(ctx)
	isize := i16(state.size * 10.0)
	font := __getFont(ctx, state.font)
	ascender = font.ascender * f32(isize / 10)
	descender = font.descender * f32(isize / 10)
	line_height = font.line_height * f32(isize / 10)
	return
}

// reset to single state
BeginState :: proc(using ctx: ^Font_Context) {
	state_count = 0
	PushState(ctx)
	ClearState(ctx)
}

// checks for texture updates after potential __getGlyph calls
EndState :: proc(using ctx: ^Font_Context) {
	// check for texture update
	if dirty_rect[0] < dirty_rect[2] && dirty_rect[1] < dirty_rect[3] {
		if callback_update != nil {
			callback_update(user_data, dirty_rect, raw_data(texture_data))
		}

		__dirtyRectReset(ctx)
	}
}