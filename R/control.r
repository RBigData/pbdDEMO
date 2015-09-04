#' A set of controls in pbdDEMO
#' 
#' This set of controls is used to provide default values in this package.
#' 
#' The elements of \code{.DEMO.CT} are default values \tabular{lcl}{ Elements
#' \tab Default \tab Usage \cr \code{gbd.major} \tab 1L \tab a default GBD
#' row-major \cr \code{ictxt} \tab 0L \tab a default BLACS context \cr
#' \code{bldim} \tab c(2L,2L) \tab a default block dimension \cr
#' \code{divide.method} \tab "block.cyclic" \tab a default balance method \cr }
#' 
#' @name DEMO Control
#' @docType data
#' @format Objects contain several default parameters for BLACS.
#' @keywords global variables
.DEMO.CT <- list(
  gbd.major = 1L,
  divide.method = c("block.cyclic", "block0"),
  ictxt = 0L,
  bldim = c(2L, 2L)
)

