# makeReportPreview - Argument Validation ---------------------------

test_that(
  "Return an error if report_instance_oid is not integerish(1)", 
  {
    expect_error(
      makeReportPreview(report_instance_oid = "1", 
                        zipfile = "file", 
                        include_image = FALSE, 
                        include_data = FALSE, 
                        build_dir = tempdir(), 
                        params = list(), 
                        report_format = "html"),
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      makeReportPreview(report_instance_oid = 1:2, 
                        zipfile = "file", 
                        include_image = FALSE, 
                        include_data = FALSE, 
                        build_dir = tempdir(), 
                        params = list(), 
                        report_format = "html"), 
      "'report_instance_oid': Must have length 1"
    )
  }
)

test_that(
  "Return an error if zipfile is not character(1)", 
  {
    expect_error(
      makeReportPreview(report_instance_oid = 1, 
                        zipfile = 123, 
                        include_image = FALSE, 
                        include_data = FALSE, 
                        build_dir = tempdir(), 
                        params = list(), 
                        report_format = "html"), 
      "'zipfile': Must be of type 'character'"
    )
    
    expect_error(
      makeReportPreview(report_instance_oid = 1, 
                        zipfile = rep("file", 2),
                        include_image = FALSE, 
                        include_data = FALSE, 
                        build_dir = tempdir(), 
                        params = list(), 
                        report_format = "html"), 
      "'zipfile': Must have length 1"
    )
  }
)

test_that(
  "Return an error if include_image is not logical(1)", 
  {
    expect_error(
      makeReportPreview(report_instance_oid = 1, 
                        zipfile = "file", 
                        include_image = "FALSE", 
                        include_data = FALSE, 
                        build_dir = tempdir(), 
                        params = list(), 
                        report_format = "html"), 
      "'include_image': Must be of type 'logical'"
    )
    
    expect_error(
      makeReportPreview(report_instance_oid = 1, 
                        zipfile = "file", 
                        include_image = c(FALSE, TRUE),
                        include_data = FALSE, 
                        build_dir = tempdir(), 
                        params = list(), 
                        report_format = "html"), 
      "'include_image': Must have length 1"
    )
  }
)

test_that(
  "Return an error if include_data is not logical(1)", 
  {
    expect_error(
      makeReportPreview(report_instance_oid = 1, 
                        zipfile = "file", 
                        include_image = FALSE, 
                        include_data = "FALSE", 
                        build_dir = tempdir(), 
                        params = list(), 
                        report_format = "html"), 
      "Variable 'include_data': Must be of type 'logical'"
    )
    
    expect_error(
      makeReportPreview(report_instance_oid = 1, 
                        zipfile = "file", 
                        include_image = FALSE, 
                        include_data = c(FALSE, TRUE), 
                        build_dir = tempdir(), 
                        params = list(), 
                        report_format = "html"), 
      "'include_data': Must have length 1"
    )
  }
)

test_that(
  "Return an error if build_dir is not character(1)", 
  {
    expect_error(
      makeReportPreview(report_instance_oid = 1, 
                        zipfile = "file", 
                        include_image = FALSE, 
                        include_data = FALSE, 
                        build_dir = 1, 
                        params = list(), 
                        report_format = "html"), 
      "'build_dir': Must be of type 'character'"
    )
    
    expect_error(
      makeReportPreview(report_instance_oid = 1, 
                        zipfile = "file", 
                        include_image = FALSE, 
                        include_data = FALSE, 
                        build_dir = rep(tempdir(), 2),
                        params = list(), 
                        report_format = "html"), 
      "'build_dir': Must have length 1"
    )
  }
)

test_that(
  "Return an error if params is not a named list", 
  {
    expect_error(
      makeReportPreview(report_instance_oid = 1, 
                        zipfile = "file", 
                        include_image = FALSE, 
                        include_data = FALSE, 
                        build_dir = tempdir(), 
                        params = mtcars, 
                        report_format = "html"), 
      "'params': Must be of type 'list'"
    )
    
    expect_error(
      makeReportPreview(report_instance_oid = 1, 
                        zipfile = "file", 
                        include_image = FALSE, 
                        include_data = FALSE, 
                        build_dir = tempdir(), 
                        params = list(letters), 
                        report_format = "html"), 
      "'params': Must have names"
    )
  }
)

test_that(
  "Return an error if report_format is not an acceptable value", 
  {
    expect_error(
      makeReportPreview(report_instance_oid = 1, 
                        zipfile = "file", 
                        include_image = FALSE, 
                        include_data = FALSE, 
                        build_dir = tempdir(), 
                        params = list(), 
                        report_format = "something different"), 
      "'report_format': Must be element of set"
    )
  }
)

# makeReportPreview - Functionality ---------------------------------

test_that(
  "Reports are generated appropriately", 
  {
    skip("Write tests for report generation after a sample template is complete")
  }
)

# generateReportFile - Argument Validation --------------------------

test_that(
  "Return an error if report_instance_oid is not integerish(1)", 
  {
    expect_error(
      generateReportFile(report_instance_oid = "1", 
                         is_preview = FALSE, 
                         is_submission = FALSE, 
                         build_dir = tempdir(), 
                         params = list(), 
                         report_format = "html"),
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      generateReportFile(report_instance_oid = 1:2, 
                         is_preview = FALSE, 
                         is_submission = FALSE, 
                         build_dir = tempdir(), 
                         params = list(), 
                         report_format = "html"), 
      "'report_instance_oid': Must have length 1"
    )
  }
)

test_that(
  "Return an error if is_preview is not logical(1)", 
  {
    expect_error(
      generateReportFile(report_instance_oid = 1, 
                         is_preview = "FALSE", 
                         is_submission = FALSE, 
                         build_dir = tempdir(), 
                         params = list(), 
                         report_format = "html"),
      "'is_preview': Must be of type 'logical'"
    )
    
    expect_error(
      generateReportFile(report_instance_oid = 1, 
                         is_preview = c(FALSE, TRUE),
                         is_submission = FALSE, 
                         build_dir = tempdir(), 
                         params = list(), 
                         report_format = "html"), 
      "'is_preview': Must have length 1"
    )
  }
)

test_that(
  "Return an error if is_submission is not logical(1)", 
  {
    expect_error(
      generateReportFile(report_instance_oid = 1, 
                         is_preview = FALSE, 
                         is_submission = "FALSE", 
                         build_dir = tempdir(), 
                         params = list(), 
                         report_format = "html"),
      "'is_submission': Must be of type 'logical'"
    )
    
    expect_error(
      generateReportFile(report_instance_oid = 1, 
                         is_preview = FALSE, 
                         is_submission = c(FALSE, TRUE),
                         build_dir = tempdir(), 
                         params = list(), 
                         report_format = "html"), 
      "'is_submission': Must have length 1"
    )
  }
)

test_that(
  "Return an error if build_dir is not character(1)", 
  {
    expect_error(
      generateReportFile(report_instance_oid = 1, 
                         is_preview = FALSE, 
                         is_submission = FALSE, 
                         build_dir = 123, 
                         params = list(), 
                         report_format = "html"),
      "'build_dir': Must be of type 'character'"
    )
    
    expect_error(
      generateReportFile(report_instance_oid = 1, 
                         is_preview = FALSE, 
                         is_submission = FALSE, 
                         build_dir = letters, 
                         params = list(), 
                         report_format = "html"), 
      "'build_dir': Must have length 1"
    )
  }
)

test_that(
  "Return an error if params is not a named list", 
  {
    expect_error(
      generateReportFile(report_instance_oid = 1, 
                         is_preview = FALSE, 
                         is_submission = FALSE, 
                         build_dir = tempdir(), 
                         params = mtcars, 
                         report_format = "html"),
      "'params': Must be of type 'list'"
    )
    
    expect_error(
      generateReportFile(report_instance_oid = 1, 
                         is_preview = FALSE, 
                         is_submission = FALSE, 
                         build_dir = tempdir(), 
                         params = list(letters), 
                         report_format = "html"), 
      "'params': Must have names"
    )
  }
)

test_that(
  "Return an error if report_format is not an acceptable option", 
  {
    expect_error(
      generateReportFile(report_instance_oid = 1, 
                         is_preview = FALSE, 
                         is_submission = FALSE, 
                         build_dir = tempdir(), 
                         params = list(), 
                         report_format = "not an option"),
      "'report_format': Must be element of set"
    )
  }
)

# generateReportFile - Functionality --------------------------------

test_that(
  "Reports are generated appropriately", 
  {
    skip("Write tests for report generation after a sample template is complete")
  }
)

# getReportImages - Argument Validation -----------------------------

test_that(
  "Return an error if report_filename is not character(1)", 
  {
    expect_error(
      getReportImages(report_filename = 123, 
                      report_format = "html"), 
      "'report_filename': Must be of type 'character'"
    )
    
    expect_error(
      getReportImages(report_filename = letters, 
                      report_format = "html"), 
      "'report_filename': Must have length 1"
    )
  }
)

test_that(
  "Return an error if report_format isn't a valid option", 
  {
    expect_error(
      getReportImages(report_filename = "file", 
                      report_format = "not an option"), 
      "'report_format': Must be element of set"
    )
  }
)

# getReportImages - Functionality -----------------------------------

test_that(
  "Images from a report can be identified", 
  {
    skip("Write tests for report generation after a sample template is complete")
  }
)

# getSupportingData - Argument Validation ---------------------------

test_that(
  "Return an error if report_filename is not character(1)", 
  {
    expect_error(
      getSupportingData(report_filename = 123, 
                        pattern = "SupportingData.csv"), 
      "'report_filename': Must be of type 'character'"
    )
    
    expect_error(
      getSupportingData(report_filename = letters, 
                        pattern = "html"), 
      "'report_filename': Must have length 1"
    )
  }
)

# getSupportingData - Functionality ---------------------------------

test_that(
  "Supporting data from a report can be identified", 
  {
    skip("Write tests for report generation after a sample template is complete")
  }
)

# zipReportFiles - Argument Validation ------------------------------

test_that(
  "Return an error if zipfile is not character(1)", 
  {
    expect_error(
      zipReportFiles(zipfile = 123, 
                     files_to_zip = letters), 
      "'zipfile': Must be of type 'character'"
    )
    
    expect_error(
      zipReportFiles(zipfile = letters, 
                     files_to_zip = letters), 
      "'zipfile': Must have length 1"
    )
  }
)

test_that(
  "Return an error if zipfile is not character", 
  {
    expect_error(
      zipReportFiles(zipfile = "file", 
                     files_to_zip = 1:10), 
      "'files_to_zip': Must be of type 'character'"
    )
  }
)

# zipReportFiles - Functionality ------------------------------------

test_that(
  "Report Files can be added to a zip file", 
  {
    skip("Write tests for report generation after a sample template is complete")
  }
)
