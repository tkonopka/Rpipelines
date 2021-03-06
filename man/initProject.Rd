% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/initProject.R
\name{initProject}
\alias{initProject}
\title{Initialize a directory structure for pipeline results}
\usage{
initProject(workdir, subdirs = c("notes", "figures", "tables", "Rda", "data",
  "code"), version = "", verbose = TRUE, forceclear = FALSE)
}
\arguments{
\item{workdir}{name of project/pipeline directory}

\item{subdirs}{subdirectories within workdir.}

\item{version}{a label indicating the version of the analysis (currently unused)}

\item{verbose}{logical. If set TRUE, function will print message to
notify when actual directories/folders are created in the file system.}

\item{forceclear}{logical. If set TRUE, function will erase any existing
files under 'workdir' and then recreate the project/pipeline directory
structure.}
}
\description{
Initialize a directory structure for pipeline results.
}

