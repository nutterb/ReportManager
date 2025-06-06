% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/queryApplicationSetting.R
\name{queryApplicationSetting}
\alias{queryApplicationSetting}
\alias{queryApplicationSettingByKey}
\title{Query Application Settings from the Database}
\usage{
queryApplicationSetting(oid = numeric(0))

queryApplicationSettingByKey(setting_key)
}
\arguments{
\item{oid}{\code{integerish(0/1)}. The OID of the setting to be retrieved.
When length is 0, all settings are returned.}

\item{setting_key}{\code{character(1)}. The setting key value for an
application setting.}
}
\description{
Provides the user with an interface to retrieve data about
application settings. One function allows retrieval by OID,
the other by the SettingKey value.
}
