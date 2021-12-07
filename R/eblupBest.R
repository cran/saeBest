#' Selecting Auxiliary Variables in Small Area Estimation (SAE) Model
#'
#' @description Select best combination of auxiliary variables with certain criterion
#' @name eblupBest
#' @param y name of response variable
#' @param x vector of auxiliary variables' name
#' @param vardir name of variance direct
#' @param criterion "loglike","AIC", "BIC", and "KIC" (default = "AIC")
#' @param data input dataset
#' @return an EBLUP-FH model with best auxiliary variables and the dataset
#' @import utils
#' sae
#' stats
utils::globalVariables(c("VAR"))
#' @export
#' @examples
#' library(saeBest)
#' example = eblupBest(y = "y", x = c("x1","x2","x3"), vardir = "var",data = data)
#'
eblupBest = function(y,x,vardir,criterion = "AIC",data) {
  comb_x = do.call(c, lapply(1:length(x), function(z) combn(x, z, simplify = FALSE)))
  names(data)[names(data) == vardir] = "VAR"
  data = data[!is.na(data[y]),]
  if(criterion == "loglike"){
    value = -10^6
    for (i in 1:length(comb_x)) {
      tryCatch({
        formula = as.formula(paste(
          paste(y,"~", collapse=""),
          paste(comb_x[[i]], collapse="+")
        ))
        sae = eblupFH(formula=formula, vardir = VAR,data=data)
        if(sae$fit$goodness[[criterion]] > value){
          value = sae$fit$goodness[[criterion]]
          sae_best = sae
        }
      }, error=function(e){
    })
    }
  }else{
    value = 10^6
    for (i in 1:length(comb_x)) {
      tryCatch({
        formula = as.formula(paste(
          paste(y,"~", collapse=""),
          paste(comb_x[[i]], collapse="+")
        ))
        sae = eblupFH(formula=formula, vardir = VAR,data=data)
        if(sae$fit$goodness[[criterion]] < value){
          value = sae$fit$goodness[[criterion]]
          sae_best = sae
        }
      }, error=function(e){
     })
    }
  }
  data_best = data[names(data) %in% c(y, row.names(sae_best$fit$estcoef)[-1])]
  return(list("eblup" = sae_best, "dataset" = data_best))
}
