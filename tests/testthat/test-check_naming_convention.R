# Unit tests for check_naming_convention_from_yaml function
test_that("check_naming_convention_from_yaml function correctly checks naming conventions", {
  file_name <- "File_Name_Test"

  conventions <- list(
    list(name = "Start with uppercase", pattern = "^[A-Z].*$", parts = 2),
    list(name = "Third part starts with uppercase", pattern = "^[A-Z].*$", parts = 3)
  )

  yaml_file <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(conventions = conventions), yaml_file)

  results <- check_naming_convention_from_yaml(file_name, yaml_file)

  expect_true(results$`Start with uppercase`)
  expect_true(results$`Third part starts with uppercase`)

  unlink(yaml_file) # Remove the temporary YAML file after testing
})


# Unit tests for check_entire_naming_convention function
test_that("check_entire_naming_convention function correctly validates entire naming convention", {
  file_name_valid <- "File_Name_Test"
  file_name_invalid <- "file_name_test"

  yaml_file <- tempfile(fileext = ".yaml")
  conventions <- list(
    list(name = "Start with uppercase", pattern = "^[A-Z].*$", parts = 2),
    list(name = "Third part starts with uppercase", pattern = "^[A-Z].*$", parts = 3)
  )
  yaml::write_yaml(list(conventions = conventions), yaml_file)

  expect_true(check_entire_naming_convention(file_name_valid, yaml_file, "underscore"))
  expect_false(check_entire_naming_convention(file_name_invalid, yaml_file, "underscore"))

  unlink(yaml_file) # Remove the temporary YAML file after testing
})
