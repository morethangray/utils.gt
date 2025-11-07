test_that(
  "make_gt_config returns list",
  { x <- make_gt_config(); expect_type(x, "list") }
  )
