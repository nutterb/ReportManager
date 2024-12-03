# Argument Validation -----------------------------------------------

test_that(
  "Return an error if report_instance_oid is not integerish(1)", 
  {
    expect_error(
      queryReportInstanceSignature(report_instance_oid = "1"),
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      queryReportInstanceSignature(report_instance_oid = 1:2),
      "'report_instance_oid': Must have length 1"
    )
  }
)

# Functionality -----------------------------------------------------

# Functionality is well covered in test-120-addReportInstanceSignature.
# No additional testing required.


# Test functionality for makeSignatureTable. It's better to do it here
# than in test-402-makeSignatureTable.R as it's possible the data will be 
# changed beyond expectation set out below.

test_that(
  "A string of HTML is returned",
  {
    Sig <- queryReportInstanceSignature(report_instance_oid = 4)
    result <- makeSignatureTable(Sig)
    
    expect_character(result)
    
    expect_equal(result,
                 "<table><tr><td><b>RoleName</b></td><td><b>SignatureName</b></td><td><b>SignatureDateTime</b></td><td><b>Button</b></td></tr><tr><td>UserAdministrator</td><td>NA</td><td>NA</td><td><button id=\"btn_reportInstanceSignature_sign_1\" type=\"button\" class=\"btn btn-default action-button\">Sign Report</button> <button class=\"btn btn-default action-button shinyjs-hide\" id=\"btn_reportInstanceSignature_remove_1\" type=\"button\">Remove Signature</button></td></tr></table>")
  }
)
