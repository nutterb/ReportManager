# Argument Validation -----------------------------------------------

test_that(
  "report_instance_oid is intgerish(1)", 
  {
    expect_error(
      queryInstanceDistributionSelection(report_instance_oid = "1"),
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      queryInstanceDistributionSelection(report_instance_oid = 1:2),
      "'report_instance_oid': Must have length 1"
    )
  }
)

# TODO: Functionality tests
