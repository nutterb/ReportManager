# Argument Validation -----------------------------------------------

test_that(
  "from_user_oid must be integerish(1)", 
  {
    expect_error(
      sendEmail(from_user_oid = "1", 
                to_address = "somewhere@email.net", 
                message = "Here is the message", 
                report_template = mtcars, 
                report_instance_oid = 1, 
                filename = tempfile(), 
                control = list(smptSErver = "smtp"), 
                embed_html = FALSE, 
                is_revision = FALSE), 
      "'from_user_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      sendEmail(from_user_oid = 1:2, 
                to_address = "somewhere@email.net", 
                message = "Here is the message", 
                report_template = mtcars, 
                report_instance_oid = 1, 
                filename = tempfile(), 
                control = list(smptSErver = "smtp"), 
                embed_html = FALSE, 
                is_revision = FALSE), 
      "'from_user_oid': Must have length 1"
    )
  }
)

test_that(
  "to_address must be character", 
  {
    expect_error(
      sendEmail(from_user_oid = 1, 
                to_address = 123, 
                message = "Here is the message", 
                report_template = mtcars, 
                report_instance_oid = 1, 
                filename = tempfile(), 
                control = list(smptSErver = "smtp"), 
                embed_html = FALSE, 
                is_revision = FALSE), 
      "'to_address': Must be of type 'character'"
    )
  }
)

test_that(
  "message must be character", 
  {
    expect_error(
      sendEmail(from_user_oid = 1, 
                to_address = "somewhere@email.net", 
                message = 123, 
                report_template = mtcars, 
                report_instance_oid = 1, 
                filename = tempfile(), 
                control = list(smptSErver = "smtp"), 
                embed_html = FALSE, 
                is_revision = FALSE), 
      "'message': Must be of type 'character'"
    )
  }
)

test_that(
  "message must be character", 
  {
    expect_error(
      sendEmail(from_user_oid = 1, 
                to_address = "somewhere@email.net", 
                message = "Here is the message", 
                report_template = "not a data frame", 
                report_instance_oid = 1, 
                filename = tempfile(), 
                control = list(smptSErver = "smtp"), 
                embed_html = FALSE, 
                is_revision = FALSE), 
      "'report_template': Must be of type 'data.frame'"
    )
  }
)

test_that(
  "report_instance_oid must be integerish(1)", 
  {
    expect_error(
      sendEmail(from_user_oid = 1, 
                to_address = "somewhere@email.net", 
                message = "Here is the message", 
                report_template = mtcars, 
                report_instance_oid = "1", 
                filename = tempfile(), 
                control = list(smptSErver = "smtp"), 
                embed_html = FALSE, 
                is_revision = FALSE), 
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      sendEmail(from_user_oid = 1, 
                to_address = "somewhere@email.net", 
                message = "Here is the message", 
                report_template = mtcars, 
                report_instance_oid = 1:2, 
                filename = tempfile(), 
                control = list(smptSErver = "smtp"), 
                embed_html = FALSE, 
                is_revision = FALSE), 
      "'report_instance_oid': Must have length 1"
    )
  }
)

test_that(
  "filename must be character", 
  {
    expect_error(
      sendEmail(from_user_oid = 1, 
                to_address = "somewhere@email.net", 
                message = "Here is the message", 
                report_template = mtcars, 
                report_instance_oid = 1, 
                filename = 123, 
                control = list(smptSErver = "smtp"), 
                embed_html = FALSE, 
                is_revision = FALSE), 
      "'filename': Must be of type 'character'"
    )
  }
)

test_that(
  "control must be a list", 
  {
    expect_error(
      sendEmail(from_user_oid = 1, 
                to_address = "somewhere@email.net", 
                message = "Here is the message", 
                report_template = mtcars, 
                report_instance_oid = 1, 
                filename = tempfile(), 
                control = mtcars, 
                embed_html = FALSE, 
                is_revision = FALSE), 
      "'control': Must be of type 'list'"
    )
  }
)

test_that(
  "embed_html must be logical(1)", 
  {
    expect_error(
      sendEmail(from_user_oid = 1, 
                to_address = "somewhere@email.net", 
                message = "Here is the message", 
                report_template = mtcars, 
                report_instance_oid = 1, 
                filename = tempfile(), 
                control = list(smptSErver = "smtp"), 
                embed_html = "FALSE", 
                is_revision = FALSE), 
      "'embed_html': Must be of type 'logical'"
    )
    
    expect_error(
      sendEmail(from_user_oid = 1, 
                to_address = "somewhere@email.net", 
                message = "Here is the message", 
                report_template = mtcars, 
                report_instance_oid = 1, 
                filename = tempfile(), 
                control = list(smptSErver = "smtp"), 
                embed_html = c(FALSE, TRUE), 
                is_revision = FALSE), 
      "'embed_html': Must have length 1"
    )
  }
)

test_that(
  "is_revision must be logical(1)", 
  {
    expect_error(
      sendEmail(from_user_oid = 1, 
                to_address = "somewhere@email.net", 
                message = "Here is the message", 
                report_template = mtcars, 
                report_instance_oid = 1, 
                filename = tempfile(), 
                control = list(smptSErver = "smtp"), 
                embed_html = FALSE, 
                is_revision = "FALSE"), 
      "'is_revision': Must be of type 'logical'"
    )
    
    expect_error(
      sendEmail(from_user_oid = 1, 
                to_address = "somewhere@email.net", 
                message = "Here is the message", 
                report_template = mtcars, 
                report_instance_oid = 1, 
                filename = tempfile(), 
                control = list(smptSErver = "smtp"), 
                embed_html = FALSE, 
                is_revision = c(FALSE, TRUE)), 
      "'is_revision': Must have length 1"
    )
  }
)
