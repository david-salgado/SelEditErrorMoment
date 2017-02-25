#' @title S4 class for the error moments in the optimization approach to selective editing
#'
#' @description Definition of the S4 class named \code{ErrorMoments} for the conditional moment  of
#' measurement errors in the optimization approach to selective editing.
#'
#'
#' @slot VarNames Character vector with the names of the variables whose error moments are contained
#'  in this object.
#'
#' @slot Domains \linkS4class{data.table} with a row per domain with the values of the variables
#' determining the domains of the data set.
#'
#' @slot Units List of data.tables with as many components as domains (in the same order of the rows
#'  of slot \code{Domains}) with every unit per domain.
#'
#' @slot Moments List of arrays with as many components as domains (in the same order of the rows
#'  of slot \code{Domains}) with the set of error moments matrices per domain. Note that the order
#'  of each error moment matrix is given by the order of the corresponding data.table in slot
#'  \code{Units}.
#'
#'
#' @examples
#' new(Class = 'ErrorMoments')
#'
#' @import data.table
#'
#' @export
setClass(Class = "ErrorMoments",
         slots = c(VarNames = 'character',
                   Domains = 'data.table',
                   Units = 'list',
                   Moments = 'list'),
         prototype = list(VarNames = character(0),
                          Domains = data.table::data.table(NULL),
                          Units = list(),
                          Moments = list()),
         validity = function(object){

           if (dim(object@Domains)[1] != length(object@Units)){

             stop('[ErrorMoments::validity] Domains and Units do not have the same number of domains.')

           }

           #ArrayCorrect <- lapply(seq(along = object@Moments), function(index.Moments){

        #   if (!is.array(object@Moments[[index.Moments]])) stop()
        #     dimArray <- dim(object@Moments[[index.Moments]])
        #     if (dim[1] != dim[2]) stop()
        #     if (dim[3] != length(object@VarNames)) stop()

        #   })
        #   if (!all(ArrayCorrect)) stop()

        #   SameUnits <- lapply(seq(along = object@Units), function(index.unitDT){

        #     if (dim(object@Units[[index.unitDT]])[[1]] != dim(object@Moments[[index.unitDT]])) stop('[contObsPredModelParam::validity] .')

        #   })


           return(TRUE)
         }
)
