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
#' ObsPredPar <- readRDS('../ObsPredPar.rds')
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                 DomainNames =  c('Tame_05._2.', 'ActivEcono_35._4._2.1.4._0'))
#'
#' AbsLossPar <- new(Class = 'AbsLossErrorMomentParam',
#'                   VarNames =  c("CifraNeg_13.___", "Personal_07.__2.__"),
#'                   Homoskedastic = c(FALSE, FALSE),
#'                   Imputation = ImpParam)
#'
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
    signature = c("contObsPredModelParam", "AbsLossErrorMomentParam"),
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
        Param@Imputation@VarNames <- paste0('Moment', VarNames)
        auxDT <- StQImputation::Impute(auxDT, Param@Imputation)

        auxDT.list <- split(auxDT, auxDT[, object@VarRoles[['Domains']], with = F])
        indexEmpty <- which(lapply(auxDT.list, function(dt){dim(dt)[1]}) == 0)
        if (length(indexEmpty) > 0) auxDT.list <- auxDT.list[-indexEmpty]


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

            nUnits <- dim(outputUnits[[indexDomain]])[1]
            nVar <- length(VarNames)
            if (nUnits > 0){
                indexMatrix <- cbind(1:nUnits, 1:nUnits, rep(1:nVar, each = nUnits))
                MomentMatrix <- matrix(NA, nrow = nUnits, ncol = nVar)
                for (indexVar in seq(along = VarNames)){

                    MomentMatrix[, indexVar] <- auxDT.list[[indexDomain]][[paste0('Moment', VarNames[indexVar])]]
                }
                out <- slam::simple_sparse_array(indexMatrix, as.vector(MomentMatrix))

            } else {

                out <- array( dim = c(0, 0, nVar))
                out <- slam::as.simple_sparse_array(out)

            }
            return(out)
        })

        output <- new(Class = 'ErrorMoments',
                      VarNames = VarNames,
                      Domains = Domains,
                      Units = outputUnits,
                      Moments = outputMoments)
        return(output)
    }
)
