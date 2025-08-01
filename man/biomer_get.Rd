% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biomer_get.R
\name{biomer_get}
\alias{biomer_get}
\title{Load Biome Raster and Legend into Environment}
\usage{
biomer_get(env = .GlobalEnv)
}
\arguments{
\item{env}{The environment into which the objects should be loaded. Defaults to \code{.GlobalEnv}.}
}
\value{
NULL (objects are assigned in the environment)
}
\description{
Loads the raster stack and associated biome legend into the global environment for use in further analysis.
}
\examples{
biomer_get()
head(biome_legend)

}
