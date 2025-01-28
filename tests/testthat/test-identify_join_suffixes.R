# Test if the function correctly identifies columns with .x or .y suffixes
test_that("identify_join_suffixes correctly identifies columns with .x or .y suffixes", {
  # Create a data frame with .x and .y suffixes
  df <- data.frame(a.x = 1:3, b.y = 4:6, c = 7:9)

  # Use the function to identify columns with .x or .y suffixes
  result <- identify_join_suffixes(df)

  # Check if the result is as expected
  expect_equal(result, c("a.x", "b.y"))
})

# Test if the function returns an empty vector when there are no .x or .y suffixes
test_that("identify_join_suffixes returns an empty vector when there are no .x or .y suffixes", {
  # Create a data frame without .x or .y suffixes
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  # Use the function to identify columns with .x or .y suffixes
  result <- identify_join_suffixes(df)

  # Check if the result is as expected
  expect_equal(result, character(0))
})

# Test if the function handles empty data frames correctly
test_that("identify_join_suffixes handles empty data frames correctly", {
  # Create an empty data frame
  df <- data.frame()

  # Use the function to identify columns with .x or .y suffixes
  result <- identify_join_suffixes(df)

  # Check if the result is as expected
  expect_equal(result, character(0))
})
