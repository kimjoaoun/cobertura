test_that("Coverage Percentage returns the right value", {

  coverage_percentage(testthat::test_path("test_xml", "badge_tst.xml"), delete_xml = FALSE) -> covr_p

  testthat::expect_equal(covr_p, 19.35)

})

