#' @title S4 class for the parameters of the computation of moments of measurement error
#'
#' @description Definition of the S4 class named \code{AbsLossErrMomParam} for the parameters for
#' the computation of error moments using an absolute value loss function in the optimization
#' approach to selective editing.
#'
#'
#' @slot VarNames Character vector with the names of the variables whose probability errors are to
#' be computed.
#'
#' @slot Homoskedastic Logical
#'
#' @slot UnitWeightNames Character vector with the names of the variables representing the unit
#' weights in heteroskedastic observation-prediction models.
#'
#' @examples
#' AbsLossPar <- new(Class = 'AbsLossErrorMomentParam',
#'                   VarNames =  c("CifraNeg_13.___", "Personal_07._2.__"),
#'                   Homoskedastic = c(FALSE, FALSE),
#'                   UnitWeightNames = character(0))
#'
#' @export
setClass(Class = "AbsLossErrorMomentParam",
         slots = c(VarNames = 'character',
                   Homoskedastic = 'logical'),
         prototype = list(VarNames = character(0),
                          Homoskedastic = logical(0)),
         validity = function(object){


           return(TRUE)
         }
)
