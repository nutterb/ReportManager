% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addEditAutoDistribution.R
\name{addEditAutoDistribution}
\alias{addEditAutoDistribution}
\title{Add or Edit Auto Distribution Configurations}
\usage{
addEditAutoDistribution(
  oid = numeric(0),
  parent_report_template,
  start_date_time,
  is_active,
  delay_after_instance_end,
  delay_units = "Hour",
  current_or_last_instance = c("Current", "LastCompleted"),
  is_add_to_archive = FALSE,
  report_format = c("html", "pdf"),
  is_distribute_internal_only = TRUE,
  is_embed_html = TRUE,
  event_user
)
}
\arguments{
\item{oid}{\code{integerish(0/1)}. The OID of the AutoDistribution object
being edited. If a zero-length value, a new object is created.}

\item{parent_report_template}{\code{integerish(1)}. The OID of the report
template.}

\item{start_date_time}{\code{POSIXct(1)}. The date and time for the
report template to begin distribution.}

\item{is_active}{\code{logical(1)}. When \code{TRUE}, the configuration is active
and distributes. When \code{FALSE}, distribution is skipped.}

\item{delay_after_instance_end}{\code{integerish(1)}. The period of time to
wait after an instance completes to attempt distribution. Delays may
be necessary to accomodate data processing tasks that must be completed
before the report is valid.}

\item{delay_units}{\code{character(1)}. The unit of time for the delay. One
of \code{c("Second", "Minute", "Hour", "Day", "Week", "Month", "Year")}}

\item{current_or_last_instance}{\code{character(1)}. One of
\verb{c("Current" or "LastCompleted")}.}

\item{is_add_to_archive}{\code{logical(1)}. When \code{TRUE}, automatically
distributed reports are also added to the archive.}

\item{report_format}{\code{character(1)}. One of \code{c("html", "pdf")}. Determines
the file format for the report attached to the e-mail.}

\item{is_distribute_internal_only}{\code{logical(1)}. When \code{TRUE}, report is
only distributed to internal users. Set to \code{FALSE} in order to include
external users as well.}

\item{is_embed_html}{\code{logical(1)}. Only applicable when
\code{report_format = "html"}. When \code{TRUE}, HTML reports are embedded into
the e-mail. When \code{FALSE}, they are attached as a file.}
}
\description{
Provides the user with the tools to add or modify the
automatic distribution configuration of report templates.
}
