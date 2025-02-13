% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/queryReportInstanceSignature.R
\name{queryReportInstanceSignature}
\alias{queryReportInstanceSignature}
\title{Retrieve Signatures on Report Instances}
\usage{
queryReportInstanceSignature(report_instance_oid)
}
\arguments{
\item{report_instance_oid}{\code{integerish(1)}. The OID of the report instance
for which signatures are being retrieved.}
}
\description{
Provides the user with the tools to retrieve signature events
for a report instance.
}
\details{
Data returned ordered in the order in which the signing role
will appear on the report. The most recent signature for each role is
sorted to the top. Inactive signature roles are sorted to the bottom.
The most recent signature is identified where \code{MostRecentSignature} equals
1.
}
