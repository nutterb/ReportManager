# Argument Validation -----------------------------------------------

test_that(
  "frequency is integerish(1)", 
  {
    expect_error(makeReportInstanceSchedule(frequency = "1", 
                                            frequency_unit = "month", 
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'frequency': Must be of type 'integerish'")
    
    expect_error(makeReportInstanceSchedule(frequency = c(1, 2), 
                                            frequency_unit = "month", 
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'frequency': Must have length 1")
  }
)

test_that(
  "frequency_unit is character(1)", 
  {
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = 123, 
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'frequency_unit': Must be element of set")
    
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = c("month", "day"),
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'frequency_unit': Must have length 1")
  }
)

test_that(
  "start_date is POSIXct(1)", 
  {
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month", 
                                            start_date = Sys.Date(), 
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'start_date': Must be of type 'POSIXct'")
    
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month", 
                                            start_date = rep(Sys.time(), 2), 
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "Variable 'start_date': Must have length 1")
  }
)

test_that(
  "offset is integerish(1)", 
  {
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month", 
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = "0", 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'offset': Must be of type 'integerish'")
    
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month", 
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = 0:1, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'offset': Must have length 1")
  }
)

test_that(
  "offset_unit is character(1)", 
  {
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month", 
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = 0, 
                                            offset_unit = 123, 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'offset_unit': Must be element of set")
    
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month",
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = 0, 
                                            offset_unit = c("month", "day"), 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'offset_unit': Must have length 1")
  }
)

test_that(
  "end_date is POSIXct(1)", 
  {
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month", 
                                            start_date = Sys.time(), 
                                            end_date = Sys.Date(),
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'end_date': Must be of type 'POSIXct'")
    
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month", 
                                            start_date = Sys.time(),
                                            end_date = rep(Sys.time(), 2),
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "Variable 'end_date': Must have length 1")
  }
)

test_that(
  "schedule_type is character(1)", 
  {
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month", 
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = 123, 
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'schedule_type': Must be of type 'character'")
    
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "day",
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = 0, 
                                            offset_unit = "month",, 
                                            schedule_type = c("none", "other"),
                                            is_offset = TRUE, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'schedule_type': Must have length 1")
  }
)

test_that(
  "is_offset is logical(1)",
  {
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month", 
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = 1, 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'is_offset': Must be of type 'logical'")
    
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "day",
                                            start_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC"), 
                                            offset = 0, 
                                            offset_unit = "month",, 
                                            schedule_type = "none", 
                                            is_offset = c(TRUE, FALSE), 
                                            index_date = as.POSIXct("2024-01-01 00:00:00", 
                                                                    tz = "UTC")), 
                 "'is_offset': Must have length 1")
  }
)

test_that(
  "index_date is POSIXct(1)", 
  {
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month", 
                                            start_date = Sys.time(), 
                                            end_date = Sys.time(),
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = Sys.Date()), 
                 "'index_date': Must be of type 'POSIXct'")
    
    expect_error(makeReportInstanceSchedule(frequency = 1, 
                                            frequency_unit = "month", 
                                            start_date = Sys.time(),
                                            end_date = Sys.time(),
                                            offset = 0, 
                                            offset_unit = "day", 
                                            schedule_type = "none", 
                                            is_offset = TRUE, 
                                            index_date = rep(Sys.time(), 2)), 
                 "Variable 'index_date': Must have length 1")
  }
)


# Functionality -----------------------------------------------------

test_that(
  "Empty schedule when the schedule_type is 'none'", 
  {
    expect_data_frame(makeReportInstanceSchedule(frequency = 1, 
                                                 frequency_unit = "month", 
                                                 start_date = Sys.time(),
                                                 end_date = Sys.time(),
                                                 offset = 0, 
                                                 offset_unit = "day", 
                                                 schedule_type = "none", 
                                                 is_offset = TRUE, 
                                                 index_date = Sys.time()), 
                      nrows = 0, 
                      ncols = 2)
  }
)

test_that(
  "Monthly Schedule with 0 offset", 
  {
    start <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
    
    Schedule <- 
      makeReportInstanceSchedule(
        frequency = 1, 
        frequency_unit = "month", 
        start_date = start,
        end_date = as.POSIXct("2023-12-31 23:59:59", tz = "UTC"),
        offset = 0, 
        offset_unit = "month", 
        schedule_type = "Monthly", 
        is_offset = TRUE, 
        index_date = Sys.time()
      )
    expect_data_frame(Schedule, 
                      nrows = 12, 
                      ncols = 2)
    
    expect_equal(Schedule$StartDate, 
                 seq(start, by = "1 month", len = 12))
  }
)

test_that(
  "Rolling Three Month Schedule - (Monthly Schedule with 3 month offset)", 
  {
    start <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
    
    Schedule <- 
      makeReportInstanceSchedule(
        frequency = 1, 
        frequency_unit = "month", 
        start_date = start,
        end_date = as.POSIXct("2023-12-31 23:59:59", tz = "UTC"),
        offset = 3, 
        offset_unit = "month", 
        schedule_type = "Rolling Three Months", 
        is_offset = TRUE, 
        index_date = Sys.time()
      )
    
    expect_data_frame(Schedule, 
                      nrows = 12, 
                      ncols = 2)
    
    all_starts <- seq(start, by = "1 month", len = 12)
    all_ends <- seq(all_starts[4], by = "1 month", len = 12)
    
    expect_equal(Schedule$StartDate, 
                 all_starts)
    
    expect_equal(Schedule$EndDate, 
                 all_ends)
  }
)

test_that(
  "Monthly Year to Date with 1 offset", 
  {
    start <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
    
    Schedule <- 
      makeReportInstanceSchedule(
        frequency = 1, 
        frequency_unit = "month", 
        start_date = start,
        end_date = as.POSIXct("2023-12-31 23:59:59", tz = "UTC"),
        offset = 1, 
        offset_unit = "year", 
        schedule_type = "Monthly (YTD)", 
        is_offset = FALSE, 
        index_date = start
      )
    
    expect_data_frame(Schedule, 
                      nrows = 12, 
                      ncols = 2)
    
    expect_equal(Schedule$StartDate, 
                 rep(start, 12))
    
    expect_equal(Schedule$EndDate, 
                 tail(seq(start, by = "1 month", len = 13), -1))
  }
)
