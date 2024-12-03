# Argument Validation -----------------------------------------------

test_that(
  "Return an error if signature_data is not a data frame",
  {
    expect_error(makeSignatureTable("character"),
                 "'signature_data': Must be of type 'data.frame'")
    
    expect_error(makeSignatureTable(mtcars),
                 "The following columns are required but not found")
  }
)

# Functionality -----------------------------------------------------

# Functionality is tested in `test-121-queryReportInstanceSignature`.
# This is done to ensure the signature data is available, otherwise
# data might be altered between that file and this file.
