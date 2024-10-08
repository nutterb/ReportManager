% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addEditReportTemplateSchedule.R
\name{addEditReportTemplateSchedule}
\alias{addEditReportTemplateSchedule}
\title{Add or Edit ReportTemplateSchedule Objects}
\usage{
addEditReportTemplateSchedule(
  oid = numeric(0),
  parent_report_template,
  parent_schedule,
  start_date,
  index_date = NA_character_,
  is_active = TRUE,
  event_user
)
}
\arguments{
\item{oid}{\code{integerish(0/1)}. The OID of the ReportTemplateSchedule
object to be edited. Use \code{numeric(0)} to add a new object.}

\item{parent_report_template}{\code{integerish(1)}. The OID of the ReportTemplate
object being associated with a disclaimer.}

\item{parent_schedule}{\code{integerish(1)}. The OID of the Schedule
object being associated with the ReportTemplate.}

\item{start_date}{\code{POSIXct(1)}. The date/time for start of the the
first instance of the report.}

\item{index_date}{\code{POSIXct(1)}. The date/time to index the start of
period-to-date style reports.}

\item{is_active}{\code{logical(1)}. When \code{TRUE}, the association will be marked
as active.}

\item{event_user}{\code{integerish(1)}. The OID of the User performing the action.}
}
\description{
Enables the user to add ReportTemplateSchedule objects
or edit an existing ReportTemplateSchedule object. Also manages the
recording of events associated with the action.
}
