% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readFiles.R
\name{buildRawProfileAPS}
\alias{buildRawProfileAPS}
\title{Copy `aps` to profiles directory, separate 'data' from 'header' as separate objects}
\usage{
buildRawProfileAPS(aps_dir)
}
\arguments{
\item{aps_dir}{}
}
\value{
nothing
}
\description{
data.aps as raw.Rda
header.aps as header.Rda
}
\examples{

buildRawProfileAPS("/data/R_data/_DHS_/aps/sample/");
buildRawProfileAPS("/data/R_data/_DHS_/aps/stage1/");

}
