#' @title S4 class for the parameters of the computation of moments of measurement error
#'
#' @description Definition of the S4 class named \code{AbsLossErrMomParam} for the parameters for
#' the computation of error moments using an absolute value loss function in the optimization
#' approach to selective editing.
#'
#'
#' @slot VarNames Character vector with the names of the variables whose moments of measurement
#' error are to be computed.
#'
#' @slot Homoskedastic \code{TRUE} (default) or \code{FALSE} indicating whether to use a
#' homoskedastic or heteroskedastic model.
#' 
#' @slot Imputation \linkS4class{ImputationParam} object with the parameters to impute missing
#' values during the computation of the moments of measurement error.
#'
#' @examples
#' AbsLossPar <- new(Class = 'AbsLossErrorMomentParam',
#'                   VarNames =  c("CifraNeg_13.___", "Personal_07._2.__"),
#'                   Homoskedastic = c(FALSE, FALSE))
#'
#' @import StQImputation
#'
#' @export
setClass(Class = "AbsLossErrorMomentParam",
         slots = c(VarNames = 'character',
                   Homoskedastic = 'logical',
                   Imputation = 'ImputationParam'),
         prototype = list(VarNames = character(0),
                          Homoskedastic = logical(0),
                          Imputation = new(Class = 'MedianImputationParam')),
         validity = function(object){


           return(TRUE)
         }
)
