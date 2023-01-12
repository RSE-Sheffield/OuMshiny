test_that("only underscores removed", {
  expect_equal(neaten_headings("_fgh"), " Fgh")
  expect_equal(neaten_headings("f_gh"), "F Gh")
  expect_equal(neaten_headings("xyz_"), "Xyz ")
  expect_equal(neaten_headings("x.y.z__"), "X.y.z")
})

test_that("title case used", {
  expect_equal(neaten_headings("abe"), "Abe")
  expect_equal(neaten_headings("ABE"), "Abe")
  expect_equal(neaten_headings("quick brown fox"), "Quick Brown Fox")
})

test_that("ITT as word capitalised", {
  expect_equal(neaten_headings("ITT_this"), "ITT This")
  expect_equal(neaten_headings("itt_this"), "ITT This")
  expect_equal(neaten_headings("other_Itt"), "Other ITT")
  expect_equal(neaten_headings("why_is_itt_this"), "Why Is ITT This")
})

test_that("ITT within word ignored", {
  expect_equal(neaten_headings("spitting"), "Spitting")
  expect_equal(neaten_headings("ittchy"), "Ittchy")
  expect_equal(neaten_headings("a_ittchy"), "A Ittchy")
  expect_equal(neaten_headings("hITT"), "Hitt")
  expect_equal(neaten_headings("hITT_"), "Hitt")
})

test_that("Can handle vectors", {
  expect_equal(
    neaten_headings(c("123", "abc", "__ff_f")),
    c("123", "Abc", "Ff F")
  )
})
