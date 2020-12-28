fn main() {
    let tommath_src = std::env::var("TOMMATHSRC").unwrap();

    cc::Build::new()
        .files(
            TOMMATH_FILES
                .iter()
                .map(|file| format!("{}/bn_{}.c", tommath_src, file)),
        )
        .define("MP_32BIT", None)
        .define("MP_FIXED_CUTOFFS", None)
        .define("MP_NO_FILE", None)
        .compile("libtommath.a");
}

// Same files listed in rts/Makefile
static TOMMATH_FILES: [&str; 56] = [
    "mp_init",
    "mp_add",
    "mp_sub",
    "mp_mul",
    "mp_zero",
    "mp_cmp",
    "mp_set_u32",
    "mp_set_i32",
    "mp_get_i32",
    "mp_get_mag_u32",
    "mp_set_u64",
    "mp_set_i64",
    "mp_get_i64",
    "mp_get_mag_u64",
    "mp_div",
    "mp_init_copy",
    "mp_neg",
    "mp_abs",
    "mp_2expt",
    "mp_expt_u32",
    "mp_set",
    "mp_sqr",
    "s_mp_add",
    "mp_cmp_mag",
    "s_mp_sub",
    "mp_grow",
    "mp_clamp",
    "mp_init_size",
    "mp_exch",
    "mp_clear",
    "mp_copy",
    "mp_count_bits",
    "mp_mul_2d",
    "mp_rshd",
    "mp_mul_d",
    "mp_div_2d",
    "mp_mod_2d",
    "s_mp_balance_mul",
    "s_mp_toom_mul",
    "s_mp_toom_sqr",
    "s_mp_karatsuba_sqr",
    "s_mp_sqr_fast",
    "s_mp_sqr",
    "s_mp_karatsuba_mul",
    "s_mp_mul_digs_fast",
    "s_mp_mul_digs",
    "mp_init_multi",
    "mp_clear_multi",
    "mp_mul_2",
    "mp_div_2",
    "mp_div_3",
    "mp_lshd",
    "mp_incr",
    "mp_decr",
    "mp_add_d",
    "mp_sub_d",
];
