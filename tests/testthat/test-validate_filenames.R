# Test case for inconsistent separator usage
test_that("Validating filenames with inconsistent separators", {
  filenames <- c("file name-1.txt", "file name 2.csv", "file name 3.doc")
  expect_false(any(validate_filenames(filenames, "space")))
})


# Test case for valid hyphen separator
test_that("Validaiting filenames with hyphen separator", {
  filenames <- c("file-name1.txt", "file-name-2.csv", "file-name3.doc")
  expect_true(all(validate_filenames(filenames, "hyphen")))
})


# Test case for valid underscore separator
test_that("Validating filenames with underscore separator", {
  filenames <- c("file_name1.txt", "file_name2.csv", "file_name_3.doc")
  expect_true(all(validate_filenames(filenames, "underscore")))
})


# Test case for inconsistent separator usage
test_that("Validating filenames with inconsistent separators", {
  filenames <- c("file name-1.txt", "file name 2.csv", "file name 3.doc")
  expect_false(any(validate_filenames(filenames, "space")))
})

# Test case for mixed separators
test_that("Validating filenames with mixed separators", {
  filenames <- c("file_name1.txt", "file-name2.csv", "file name3.doc")
  expect_false(any(validate_filenames(filenames, "hyphen")))
  expect_false(any(validate_filenames(filenames, "underscore")))
  expect_false(any(validate_filenames(filenames, "space")))
})


# Test case for inconsistent separator usage
test_that("Validating filenames with inconsistent separators", {
  filenames <- c("file name-1.txt", "file name 2.csv", "file name 3.doc")
  expect_false(validate_filenames(filenames, "space"))
})
