test_that("Mixed bit-field struct parsing and access", {
    skip("working on ffi_parse_header right now")
    header_text <- "typedef struct hFILE {\n    char *buffer, *begin, *end, *limit;\n    const struct hFILE_backend *backend;\n    off_t offset;\n    unsigned at_eof:1, mobile:1, readonly:1, preserve:1;\n    int has_errno;\n  } hFILE;\n"
    tmp <- tempfile(fileext = ".h")
    writeLines(header_text, tmp)
    parsed <- ffi_parse_header(tmp)
    code <- generate_r_bindings(parsed)
    tmpf <- tempfile(fileext = ".R")
    writeLines(code, tmpf)
    source(tmpf)

    expect_true(exists("hFILE"))
    # Create instance and set bits via direct member name
    hp <- ffi_alloc(hFILE)
    ffi_set_field(hp, "at_eof", 1L, hFILE)
    val <- ffi_get_field(hp, "at_eof", hFILE)
    expect_equal(as.integer(val), 1L)
    # Set another bit
    ffi_set_field(hp, "mobile", 1L, hFILE)
    expect_equal(as.integer(ffi_get_field(hp, "mobile", hFILE)), 1L)

    # Setting and getting a regular field still works
    ffi_set_field(hp, "has_errno", 42L, hFILE)
    expect_equal(as.integer(ffi_get_field(hp, "has_errno", hFILE)), 42L)
})
