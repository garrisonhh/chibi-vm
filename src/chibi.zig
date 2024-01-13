//! the 'chibi.h' header translated and edited for zig usage

pub extern var include_paths: StringArray;
pub extern var opt_fpic: bool;
pub extern var opt_fcommon: bool;
pub extern var base_file: [*:0]u8;

// utils =======================================================================

pub extern fn format(fmt: [*:0]u8, ...) [*:0]u8;
pub extern fn align_to(n: c_int, @"align": c_int) c_int;
pub extern fn encode_utf8(buf: [*]u8, c: u32) c_int;
pub extern fn decode_utf8(new_pos: *[*:0]u8, p: [*:0]u8) u32;
pub extern fn is_ident1(c: u32) bool;
pub extern fn is_ident2(c: u32) bool;
pub extern fn display_width(p: [*]u8, len: c_int) c_int;
pub extern fn file_exists(path: [*:0]u8) bool;

// string array ================================================================

pub const StringArray = extern struct {
    data: ?[*][*:0]u8,
    capacity: c_int,
    len: c_int,
};

pub extern fn strarray_push(arr: *StringArray, s: [*:0]u8) void;

// hashmap =====================================================================

pub const HashEntry = extern struct {
    key: [*:0]u8,
    keylen: c_int,
    val: ?*anyopaque,
};

pub const HashMap = extern struct {
    buckets: ?[*]HashEntry,
    capacity: c_int,
    used: c_int,
};

pub extern fn hashmap_get(map: *HashMap, key: [*:0]u8) ?*anyopaque;
pub extern fn hashmap_get2(map: *HashMap, key: [*:0]u8, keylen: c_int) ?*anyopaque;
pub extern fn hashmap_put(map: *HashMap, key: [*:0]u8, val: ?*anyopaque) void;
pub extern fn hashmap_put2(map: *HashMap, key: [*:0]u8, keylen: c_int, val: ?*anyopaque) void;
pub extern fn hashmap_delete(map: *HashMap, key: [*:0]u8) void;
pub extern fn hashmap_delete2(map: *HashMap, key: [*:0]u8, keylen: c_int) void;
pub extern fn hashmap_test() void;

// errors ======================================================================

pub extern fn @"error"(fmt: [*:0]u8, ...) void;
pub extern fn error_at(loc: [*:0]u8, fmt: [*:0]u8, ...) void;
pub extern fn error_tok(tok: *Token, fmt: [*:0]u8, ...) void;
pub extern fn warn_tok(tok: *Token, fmt: [*:0]u8, ...) void;

// tokenization ================================================================

pub const Hideset = opaque {};

pub const TokenKind = enum(c_uint) {
    ident = 0,
    punct = 1,
    keyword = 2,
    str = 3,
    num = 4,
    pp_num = 5,
    eof = 6,
};

pub const Token = extern struct {
    kind: TokenKind,
    next: ?*Token,
    val: i64,
    fval: c_longdouble,
    loc: [*:0]u8,
    len: c_int,
    ty: *Type,
    str: [*:0]u8,
    file: *File,
    filename: [*:0]u8,
    line_no: c_int,
    line_delta: c_int,
    at_bol: bool,
    has_space: bool,
    hideset: ?*Hideset,
    origin: *Token,
};

pub const File = extern struct {
    name: [*:0]const u8,
    file_no: c_int,
    contents: [*:0]const u8,
    display_name: [*:0]u8,
    line_delta: c_int,
};

pub extern fn equal(tok: [*c]Token, op: [*c]u8) bool;
pub extern fn skip(tok: [*c]Token, op: [*c]u8) [*c]Token;
pub extern fn consume(rest: [*c][*c]Token, tok: [*c]Token, str: [*c]u8) bool;
pub extern fn convert_pp_tokens(tok: [*c]Token) void;
pub extern fn get_input_files() [*c][*c]File;
pub extern fn new_file(name: [*:0]const u8, file_no: c_int, contents: [*:0]const u8) *File;
pub extern fn tokenize_string_literal(tok: [*c]Token, basety: [*c]Type) ?*Token;
pub extern fn tokenize(file: *File) ?*Token;
pub extern fn tokenize_file(filename: [*c]u8) ?*Token;
pub extern fn search_include_paths(filename: [*c]u8) [*c]u8;

// preprocessor ================================================================

pub extern fn init_macros() void;
pub extern fn define_macro(name: [*:0]u8, buf: [*:0]u8) void;
pub extern fn undef_macro(name: [*:0]u8) void;
pub extern fn preprocess(tok: *Token) *Token;

// parsing =====================================================================

pub const Relocation = extern struct {
    next: ?*Relocation,
    offset: c_int,
    label: [*c][*c]u8,
    addend: c_long,
};

pub const Obj = extern struct {
    next: ?*Obj,
    name: [*:0]u8,
    ty: *Type,
    tok: ?*Token,
    is_local: bool,
    @"align": c_int,
    offset: c_int,
    is_function: bool,
    is_definition: bool,
    is_static: bool,
    is_tentative: bool,
    is_tls: bool,
    init_data: ?[*]u8,
    rel: ?*Relocation,
    is_inline: bool,
    params: ?*Obj,
    body: ?*Node,
    locals: ?*Obj,
    va_area: ?*Obj,
    alloca_bottom: ?*Obj,
    stack_size: c_int,
    is_live: bool,
    is_root: bool,
    refs: StringArray,
};

pub const NodeKind = enum(c_uint) {
    null_expr = 0,
    add = 1,
    sub = 2,
    mul = 3,
    div = 4,
    neg = 5,
    mod = 6,
    bitand = 7,
    bitor = 8,
    bitxor = 9,
    shl = 10,
    shr = 11,
    eq = 12,
    ne = 13,
    lt = 14,
    le = 15,
    assign = 16,
    cond = 17,
    comma = 18,
    member = 19,
    addr = 20,
    deref = 21,
    not = 22,
    bitnot = 23,
    logand = 24,
    logor = 25,
    @"return" = 26,
    @"if" = 27,
    @"for" = 28,
    do = 29,
    @"switch" = 30,
    case = 31,
    block = 32,
    goto = 33,
    goto_expr = 34,
    label = 35,
    label_val = 36,
    funcall = 37,
    expr_stmt = 38,
    stmt_expr = 39,
    @"var" = 40,
    vla_ptr = 41,
    num = 42,
    cast = 43,
    memzero = 44,
    @"asm" = 45,
    cas = 46,
    exch = 47,
};

pub const Node = extern struct {
    kind: NodeKind,
    next: ?*Node,
    ty: ?*Type,
    tok: ?*Token,
    lhs: ?*Node,
    rhs: ?*Node,
    cond: ?*Node,
    then: ?*Node,
    els: ?*Node,
    init: ?*Node,
    inc: ?*Node,
    brk_label: [*c]u8,
    cont_label: [*c]u8,
    body: ?*Node,
    member: [*c]Member,
    func_ty: [*c]Type,
    args: ?*Node,
    pass_by_stack: bool,
    ret_buffer: [*c]Obj,
    label: [*c]u8,
    unique_label: [*c]u8,
    goto_next: [*c]Node,
    case_next: [*c]Node,
    default_case: [*c]Node,
    begin: c_long,
    end: c_long,
    asm_str: [*c]u8,
    cas_addr: [*c]Node,
    cas_old: [*c]Node,
    cas_new: [*c]Node,
    atomic_addr: [*c]Obj,
    atomic_expr: [*c]Node,
    @"var": ?*Obj,
    val: u64,
    fval: c_longdouble,
};

pub extern fn add_type(node: *Node) void;
pub extern fn new_cast(expr: *Node, ty: *Type) *Node;
pub extern fn const_expr(rest: **Token, tok: *Token) i64;
pub extern fn parse(tok: *Token) ?*Obj;

// type system =================================================================

pub const Member = extern struct {
    next: ?*Member,
    ty: *Type,
    tok: *Token,
    name: *Token,
    idx: c_int,
    @"align": c_int,
    offset: c_int,
    is_bitfield: bool,
    bit_offset: c_int,
    bit_width: c_int,
};

pub const TypeKind = enum(c_uint) {
    void = 0,
    bool = 1,
    char = 2,
    short = 3,
    int = 4,
    long = 5,
    float = 6,
    double = 7,
    ldouble = 8,
    @"enum" = 9,
    ptr = 10,
    func = 11,
    array = 12,
    vla = 13,
    @"struct" = 14,
    @"union" = 15,
};

pub const Type = extern struct {
    kind: TypeKind,
    size: c_int,
    @"align": c_int,
    is_unsigned: bool,
    is_atomic: bool,
    origin: *Type,
    base: ?*Type,
    name: ?*Token,
    name_pos: ?*Token,
    array_len: c_int,
    vla_len: ?*Node,
    vla_size: ?*Obj,
    members: ?*Member,
    is_flexible: bool,
    is_packed: bool,
    return_ty: ?*Type,
    params: ?*Type,
    is_variadic: bool,
    next: ?*Type,
};

pub extern var ty_void: *Type;
pub extern var ty_bool: *Type;
pub extern var ty_char: *Type;
pub extern var ty_short: *Type;
pub extern var ty_int: *Type;
pub extern var ty_long: *Type;
pub extern var ty_uchar: *Type;
pub extern var ty_ushort: *Type;
pub extern var ty_uint: *Type;
pub extern var ty_ulong: *Type;
pub extern var ty_float: *Type;
pub extern var ty_double: *Type;
pub extern var ty_ldouble: *Type;

pub extern fn is_integer(ty: *Type) bool;
pub extern fn is_flonum(ty: *Type) bool;
pub extern fn is_numeric(ty: *Type) bool;
pub extern fn is_compatible(t1: *Type, t2: *Type) bool;
pub extern fn copy_type(ty: *Type) *Type;
pub extern fn pointer_to(base: *Type) *Type;
pub extern fn func_type(return_ty: *Type) *Type;
pub extern fn array_of(base: *Type, size: c_int) *Type;
pub extern fn vla_of(base: *Type, expr: *Node) *Type;
pub extern fn enum_type() *Type;
pub extern fn struct_type() *Type;

// codegen =====================================================================

// pub extern fn codegen(prog: *Obj, out: *FILE) void;
