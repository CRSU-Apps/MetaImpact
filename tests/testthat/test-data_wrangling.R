
test_that(".FixColumnNameCases() fixes cases for continuous long data", {
  data <- CleanData(read.csv("data/AntiVEGF_Continuous_Pairwise_Long.csv"))
  names(data) <- c("sTuDy", "t", "n", "mEaN", "sD")
  allowed_names = c("Study", "T", "N", "Mean", "SD")
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data)
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that(".FixColumnNameCases() fixes cases for continuous wide data", {
  data <- CleanData(read.csv("data/AntiVEGF_Continuous_Pairwise_Wide.csv"))
  
  arm_fields = c("t", "n", "mEaN", "sD")
  names(data) <- c("sTuDy", paste0(arm_fields, ".1"), paste0(arm_fields, ".2"))
  
  allowed_arm_fields = c("T", "N", "Mean", "SD")
  allowed_names = c("Study", paste0(allowed_arm_fields, ".1"), paste0(allowed_arm_fields, ".2"))
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data)
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that(".FixColumnNameCases() fixes cases for binary long data", {
  data <- CleanData(read.csv("data/AntiVEGF_Binary_Pairwise_Long.csv"))
  names(data) <- c("sTuDy", "t", "r", "n")
  allowed_names = c("Study", "T", "R", "N")
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data)
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that(".FixColumnNameCases() fixes cases for binary wide data", {
  data <- CleanData(read.csv("data/AntiVEGF_Binary_Pairwise_Wide.csv"))
  
  arm_fields = c("t", "r", "n")
  names(data) <- c("sTuDy", paste0(arm_fields, ".1"), paste0(arm_fields, ".2"))
  
  allowed_arm_fields = c("T", "R", "N")
  allowed_names = c("Study", paste0(allowed_arm_fields, ".1"), paste0(allowed_arm_fields, ".2"))
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data)
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that(".AddStudyIds() adds study IDs for continuous long data", {
  data <- CleanData(read.csv("data/AntiVEGF_Continuous_Pairwise_Long.csv"))
  
  wrangled_data <- .AddStudyIds(data)
  
  expect_equal(colnames(wrangled_data), c("StudyID", colnames(data)),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(c("StudyID", colnames(data))))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 2, 2, 3, 3, 4, 4),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data)
})

test_that(".AddStudyIds() adds study IDs for continuous wide data", {
  data <- CleanData(read.csv("data/AntiVEGF_Continuous_Pairwise_Wide.csv"))
  
  wrangled_data <- .AddStudyIds(data)
  
  expect_equal(colnames(wrangled_data), c("StudyID", colnames(data)),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(c("StudyID", colnames(data))))
  
  expect_equal(wrangled_data$StudyID, 1:4,
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data)
})

test_that(".AddStudyIds() adds study IDs for binary long data", {
  data <- CleanData(read.csv("data/AntiVEGF_Binary_Pairwise_Long.csv"))
  
  wrangled_data <- .AddStudyIds(data)
  
  expect_equal(colnames(wrangled_data), c("StudyID", colnames(data)),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(c("StudyID", colnames(data))))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data)
})

test_that(".AddStudyIds() adds study IDs for binary wide data", {
  data <- CleanData(read.csv("data/AntiVEGF_Binary_Pairwise_Wide.csv"))
  
  wrangled_data <- .AddStudyIds(data)
  
  expect_equal(colnames(wrangled_data), c("StudyID", colnames(data)),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(c("StudyID", colnames(data))))
  
  expect_equal(wrangled_data$StudyID, 1:9,
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data)
})

test_that("WrangleUploadData() wrangles continuous long data to be usable in the rest of the app", {
  data <- CleanData(read.csv("data/AntiVEGF_Continuous_Pairwise_Long.csv"))
  
  wrangled_data <- WrangleUploadData(data)
  
  expect_equal(colnames(wrangled_data), c("StudyID", "Study", "T", "N", "Mean", "SD"),
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 2, 2, 3, 3, 4, 4),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Contents of columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data)
})

test_that("WrangleUploadData() wrangles continuous wide data to be usable in the rest of the app", {
  data <- CleanData(read.csv("data/AntiVEGF_Continuous_Pairwise_Wide.csv"))
  
  wrangled_data <- WrangleUploadData(data)

  expected_columns <- c(
    "StudyID",
    "Study",
    "T.1",
    "N.1",
    "Mean.1",
    "SD.1",
    "T.2",
    "N.2",
    "Mean.2",
    "SD.2"
  )

  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, 1:4,
               label = format_vector_to_string(wrangled_data$StudyID))

  # Contents of columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data)
})

test_that("WrangleUploadData() wrangles binary long data to be usable in the rest of the app", {
  data <- CleanData(read.csv("data/AntiVEGF_Binary_Pairwise_Long.csv"))
  
  wrangled_data <- WrangleUploadData(data)
  
  expect_equal(colnames(wrangled_data), c("StudyID", "Study", "T", "R", "N"),
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8 , 8 , 9, 9),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Contents of columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data)
})

test_that("WrangleUploadData() wrangles binary wide data to be usable in the rest of the app", {
  data <- CleanData(read.csv("data/AntiVEGF_Binary_Pairwise_Wide.csv"))
  
  wrangled_data <- WrangleUploadData(data)
  
  expected_columns <- c(
    "StudyID",
    "Study",
    "T.1",
    "R.1",
    "N.1",
    "T.2",
    "R.2",
    "N.2"
  )
  
  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, 1:9,
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Contents of columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data)
})
