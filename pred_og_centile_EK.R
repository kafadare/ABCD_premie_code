pred_og_centile_EK <- function(gamlssModel, og.data, get.zscores = FALSE, new.data=NULL){
  pheno <- gamlssModel$mu.terms[[2]]
  
  #subset df cols just to predictors from model
  predictor_list <- list_predictors(gamlssModel)
  stopifnot("Dataframe columns and model covariates don't match" = 
              predictor_list %in% names(og.data))
  if (is.null(new.data)) {
    newData <- subset(og.data, select = names(og.data) %in% predictor_list)
  } else {
    stopifnot("Dataframe columns and model covariates don't match" = 
                predictor_list %in% names(new.data))
    newData <- subset(new.data, select = names(new.data) %in% predictor_list)
    #make sure all vals are within range of those originally modeled
    #stopifnot(check_range(subset(og.data, select = names(og.data) %in% predictor_list), newData) == TRUE) 
  }
  
  #predict
  predModel <- predictAll(gamlssModel, newdata=newData, data=og.data, type= "response")
  
  #get dist type (e.g. GG, BCCG) and write out function
  fname <- gamlssModel$family[1]
  pfun <- paste0("p", fname)
  
  #look for moments
  has_sigma <- "sigma" %in% gamlssModel[[2]]
  has_nu <- "nu" %in% gamlssModel[[2]]
  has_tau <- "tau" %in% gamlssModel[[2]]
  
  centiles <- c()
  #iterate through participants
  if (is.null(new.data)) {
    for (i in 1:nrow(og.data)){
      cent_args <- list(og.data[[pheno]][[i]], predModel$mu[[i]])
      
      if (has_sigma){
        cent_args$sigma <- predModel$sigma[[i]]
      }
      if (has_nu){
        cent_args$nu <- predModel$nu[[i]]
      } 
      if (has_tau){
        cent_args$tau <- predModel$tau[[i]]
      } 
      
      centiles[i] <- do.call(pfun, cent_args)
      
      #don't let centile = 1 (for z-scores)!
      if (centiles[i] == 1) {
        centiles[i] <- 0.99999999999999994 #largest number i could get w/o rounding to 1 (trial & error)
      }
      #don't let centile = 0 (for z-scores)!
      if (centiles[i] == 0) {
        centiles[i] <- 0.0000000000000000000000001 #25 dec places
      }
      
    }
  } else {
    #iterate through participants
    for (i in 1:nrow(new.data)){
      cent_args <- list(new.data[[pheno]][[i]], predModel$mu[[i]])
      
      if (has_sigma){
        cent_args$sigma <- predModel$sigma[[i]]
      }
      if (has_nu){
        cent_args$nu <- predModel$nu[[i]]
      } 
      if (has_tau){
        cent_args$tau <- predModel$tau[[i]]
      } 
      
      centiles[i] <- do.call(pfun, cent_args)
      
      #don't let centile = 1 (for z-scores)!
      if (centiles[i] == 1) {
        centiles[i] <- 0.99999999999999994 #largest number i could get w/o rounding to 1 (trial & error)
      }
      #don't let centile = 0 (for z-scores)!
      if (centiles[i] == 0) {
        centiles[i] <- 0.0000000000000000000000001 #25 dec places
      }
      
    }
  }
  if (get.zscores == FALSE){
    return(centiles)
  } else {
    #check to make sure distribution family is LMS
    if (fname %in% c("BCCG", "NO")){
      
      #get z scores from normed centiles - how z.score() does it
      rqres <- qnorm(centiles)
      
      #return dataframe
      df <- data.frame("centile" = centiles,
                       "z_score" = rqres)
      return(df)
    } else {
      stop(paste("This distribution family is not supported for calculating z scores.", 
                 "\n If you think this message was returned in error, update code to include appropriate dist. families.", ""))
    }
  }
}

cent_cdf_EK <- function (gamlssModel, df, group = NULL, interval_var = NULL, 
          ...) 
{
  df$centile <- pred_og_centile_EK(gamlssModel, df)
  
  if (!is.null(group) && is.numeric(df[[group]])) {
    df[[group]] <- as.factor(df[[group]])
  }
  if (!is.null(interval_var)) {
    df$Interval <- cut_interval(df[[interval_var]], ...)
    df <- df %>% group_by(Interval)
  }
  df %>% summarise(`1%` = round(sum(centile <= 0.01)/n(), digits = 3), 
                   `5%` = round(sum(centile <= 0.05)/n(), digits = 3), `10%` = round(sum(centile <= 
                                                                                           0.1)/n(), digits = 3), `25%` = round(sum(centile <= 
                                                                                                                                      0.25)/n(), digits = 3), `50%` = round(sum(centile <= 
                                                                                                                                                                                  0.5)/n(), digits = 3), `75%` = round(sum(centile <= 
                                                                                                                                                                                                                             0.75)/n(), digits = 3), `90%` = round(sum(centile <= 
                                                                                                                                                                                                                                                                         0.9)/n(), digits = 3), `95%` = round(sum(centile <= 
                                                                                                                                                                                                                                                                                                                    0.95)/n(), digits = 3), `99%` = round(sum(centile <= 
                                                                                                                                                                                                                                                                                                                                                                0.99)/n(), digits = 3), .by = !!group)
}