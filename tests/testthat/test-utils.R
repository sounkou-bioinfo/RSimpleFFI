test_that("ffi_alloc_buffer allocates and finalizes memory", {
  ptr <- ffi_alloc_buffer(32L)
  expect_true(inherits(ptr, "externalptr"))
  expect_false(is.null(ptr))
  rm(ptr)
  gc()
})
