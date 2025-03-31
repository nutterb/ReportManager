# Argument Validation -----------------------------------------------

test_that(
  "report_instance_oid is integerish(1)", 
  {
    expect_error(
      submitReport(report_instance_oid = "1", 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      submitReport(report_instance_oid = 1:2, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'report_instance_oid': Must have length 1"
    )
  }
)

test_that(
  "is_submission is logical(1)", 
  {
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = "FALSE", 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'is_submission': Must be of type 'logical'"
    )
    
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = c(FALSE, TRUE), 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'is_submission': Must have length 1"
    )
  }
)

test_that(
  "is_distribute is logical(1)", 
  {
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = "FALSE", 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'is_distribute': Must be of type 'logical'"
    )
    
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = c(FALSE, TRUE), 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'is_distribute': Must have length 1"
    )
  }
)

test_that(
  "is_distribute_internal_only is logical(1)", 
  {
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = "FALSE",
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'is_distribute_internal_only': Must be of type 'logical'"
    )
    
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = c(FALSE, TRUE),
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'is_distribute_internal_only': Must have length 1"
    )
  }
)

test_that(
  "is_add_to_archive is logical(1)", 
  {
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = "FALSE", 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'is_add_to_archive': Must be of type 'logical'"
    )
    
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = c(FALSE, TRUE), 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'is_add_to_archive': Must have length 1" 
    )
  }
)

test_that(
  "is_embed_html is logical(1)", 
  {
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = "FALSE",
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'is_embed_html': Must be of type 'logical'"
    )
    
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = c(FALSE, TRUE),
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'is_embed_html': Must have length 1"
    )
  }
)

test_that(
  "params is a named list", 
  {
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = 123, 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'params': Must be of type 'list'"
    )
    
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(123, 
                                 "abc"), 
                   report_format = "html", 
                   current_user_oid = 1), 
      "'params': Must have names"
    )
  }
)

test_that(
  "report_format must be one of c('pdf', 'html')", 
  {
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "not a valid option", 
                   current_user_oid = 1), 
      "'report_format': Must be element of set"
    )
  }
)

test_that(
  "current_oid is integerish(1)", 
  {
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = "1"), 
      "'current_user_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      submitReport(report_instance_oid = 1, 
                   is_submission = FALSE, 
                   is_distribute = FALSE, 
                   is_distribute_internal_only = FALSE,
                   is_add_to_archive = FALSE, 
                   is_embed_html = FALSE,
                   params = list(), 
                   report_format = "html", 
                   current_user_oid = 1:2), 
      "'current_user_oid': Must have length 1"
    )
  }
)

# TODO: Functionality tests
