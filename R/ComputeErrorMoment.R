#' \code{ComputeErrorMoments} computes the moments of measurement error for each statistical unit
#'
#' @param object Object of class \linkS4class{contObsPredModelParam} containing the statistical
#' units whose moments of measurement error for each variable is to be computed.
#'
#' @param Param Object of virtual class \linkS4class{ErrorMomentParam} with the parameters determining
#' the method of computation of the moments of each statistical unit.
#'
#' @return Object of class \linkS4class{ErrorMoments} with the measurement error computed for each
#' variable and each statistical unit.
#'
#'
#' @examples
#' \dontrun{
#'
#' AbsLossPar <- new(Class = 'AbsLossErrorMomentParam',
#'                   VarNames =  c("CifraNeg_13.___", "Personal_07.__2.__"),
#'                   Homoskedastic = c(FALSE, FALSE))
#'
#' ComputeErrorMoment(ObsPredPar, AbsLossPar)
#'
#' }
#' @include ErrorMomentParam-class.R
#'
#' @import data.table contObsPredModelParam
#'
#' @export
setGeneric("ComputeErrorMoment", function(object, Param) {standardGeneric("ComputeErrorMoment")})

#' @rdname ComputeErrorMoment
#'
#'@export
setMethod(
    f = "ComputeErrorMoment",
    signature = c("contObsPredModelParam", "ErrorMomentParam"),
    function(object, Param){

        auxDT <- StQ::dcast_StQ(object@Data)
        VarNames <- Param@VarNames

        lapply(seq(along = Param@VarNames), function(indexVar){

            VarName <- VarNames[indexVar]
            auxDT[, (paste0('Moment', VarName)) := SelEditFunctions::AbsLossErrorMoment(
                auxDT[[VarName]],
                auxDT[[paste0('Pred', VarName)]],
                auxDT[[paste0('PredErrorSTD', VarName)]],
                auxDT[[paste0('ObsErrorSTD', VarName)]],
                auxDT[[paste0('DesignW', VarName)]],
                auxDT[[paste0('ErrorProb', VarName)]],
                Param@Homoskedastic[indexVar])]

        })

        auxDT.list <- split(auxDT, auxDT[[object@VarRoles[['Domains']]]])
        outputDomains <- lapply(seq(along = auxDT.list), function(indexDomain){

            out <- auxDT.list[[indexDomain]][, object@VarRoles[['Domains']], with = FALSE]
            setkeyv(out, names(out))
            out <- out[!duplicated(out)]
            return(out)
        })
        Domains <- rbindlist(outputDomains)
        outputUnits <- lapply(seq(along = auxDT.list), function(indexDomain){

            out <- auxDT.list[[indexDomain]][, object@VarRoles[['Units']], with = FALSE]
            return(out)
        })
        outputMoments <- lapply(seq(along = auxDT.list), function(indexDomain){

            localMoments <- auxDT.list[[indexDomain]][, paste0('Moment', VarNames), with = FALSE]

            out <- auxDT.list[[indexDomain]][, object@VarRoles[['Units']], with = FALSE]
            return(out)
        })

        return(outputDomains)
    }
)