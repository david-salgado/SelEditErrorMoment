#' \code{ComputeErrorMoments} computes the moments of measurement error for each statistical unit
#'
#' @param object Object of class \linkS4class{contObsPredModelParam} containing the statistical
#' units whose moments of measurement error for each variable is to be computed.
#'
#' @param Param Object of virtual class \linkS4class{ErrorMomentParam} with the parameters
#' determining the method of computation of the moments of each statistical unit.
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
#' @import data.table contObsPredModelParam categObsPredModelParam
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

        lapply(seq(along = VarNames), function(indexVar){

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

#'@export
setMethod(
    f = "ComputeErrorMoment",
    signature = c("categObsPredModelParam", "AbsLossErrorMomentParam"),
    function(object, Param){

        data.StQ <- getData(object)
        IDQuals <- getIDQual(data.StQ)
        regressors <- getRegressors(object)
        regressands <- getRegressands(object)
        wName <- object@VarRoles$DesignW
        domains <- object@VarRoles$Domains
        data.dt <- dcast_StQ(data.StQ)
        data.dt <- data.dt[, setdiff(names(data.dt), c(regressors, regressands)), with = FALSE]
        probs.dt <- object@probs
        workingDT <- merge(data.dt, probs.dt, all.x = TRUE)

        if (Param@Homoskedastic == TRUE){


            workingDT <- workingDT[
                , Moment := ecdf(as.numeric(get(wName)) * P01)(as.numeric(get(wName)) * P01),
                by = c(domains, regressands)]

        }

        if (Param@Homoskedastic == FALSE){

            workingDT <- workingDT[
                , moment := ecdf(P01)(P01),
                by = c(domains, regressands)]

        }

        IDQuals <- getIDQual(data.StQ)
        domains <- c(domains, Param@Imputation@DomainNames)
        workingVars <- c(IDQuals, domains, 'variable', regressands, regressors, 'Moment')
        workingDT <- workingDT[, ..workingVars]

        if (length(domains) > 0) {

            formula <- paste(
                paste(paste(IDQuals, collapse = ' + '),
                      paste(domains, collapse = ' + '),
                      paste(regressors, collapse = ' + '),
                      sep = ' + '),
                'variable', sep = ' ~ ')

        } else {

            formula <- paste(
                paste(paste(IDQuals, collapse = ' + '),
                      paste(regressors, collapse = ' + '),
                      sep = ' + '),
                'variable', sep = ' ~ ')
        }


        auxDT <- dcast(workingDT, formula = as.formula(formula), value.var = 'Moment')
        setnames(auxDT, regressands, paste0('Moment', regressands))
        Param@Imputation@VarNames <- paste0('Moment', regressands)
        auxDT <- StQImputation::Impute(auxDT, Param@Imputation)
        auxDT[, (domains) := NULL]
        domains <- object@VarRoles$Domains

        if (length(domains) > 0) {

            auxDT.list <- split(auxDT, auxDT[, ..domains])
            indexEmpty <- which(lapply(auxDT.list, function(dt){dim(dt)[1]}) == 0)
            if (length(indexEmpty) > 0) auxDT.list <- auxDT.list[-indexEmpty]

            outputDomains <- lapply(seq(along = auxDT.list), function(indexDomain){

                out <- auxDT.list[[indexDomain]][, ..domains]
                setkeyv(out, names(out))
                out <- out[!duplicated(out)]
                return(out)
            })
            outputDomains <- rbindlist(outputDomains)

        } else {

            auxDT.list <- list(auxDT)
            outputDomains <- data.table(NULL)

        }


        outputUnits <- lapply(seq(along = auxDT.list), function(indexDomain){

            out <- auxDT.list[[indexDomain]][, ..IDQuals]
            return(out)
        })

        outputMoments <- lapply(seq(along = auxDT.list), function(indexDomain){

            nUnits <- dim(outputUnits[[indexDomain]])[1]
            nVar <- length(regressands)
            if (nUnits > 0) {

                indexMatrix <- cbind(1:nUnits, 1:nUnits, rep(1:nVar, each = nUnits))
                MomentMatrix <- matrix(NA, nrow = nUnits, ncol = nVar)
                for (indexVar in seq(along = regressands)){

                    MomentMatrix[, indexVar] <- auxDT.list[[indexDomain]][[paste0('Moment', regressands[indexVar])]]
                }
                out <- slam::simple_sparse_array(indexMatrix, as.vector(MomentMatrix))

            } else {

                out <- array( dim = c(0, 0, nVar))
                out <- slam::as.simple_sparse_array(out)

            }
            return(out)
        })




        output <- new(Class = 'ErrorMoments',
                      VarNames = regressands,
                      Domains = outputDomains,
                      Units = outputUnits,
                      Moments = outputMoments)
        return(output)
    }
)

