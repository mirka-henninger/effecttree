#' Calculates the Mantel-Haenszel statistic for Differential Item Functioning (DIF)
#'
#' @param dat A dataframe with dichotomous item responses
#' @param split_group An indicator of the two groups for which DIF analyses should be performed
#' @param sums A sum score used as a matching criterion for computing the Mantel Haenszel statistic
#' @param purification A character indicating the type of purification that is used on the Mantel Haenszel statistic. Options are "none", "2step", or "iterative"
#' @param ... Further arguments
#'
#' @return A list with the Mantel-Haenszel effect size measure in the Delta scale, the classification based on the ETS guidelines and information on purification and convergence of purification if iterative purification was done.
calculate_mantelhaenszel <- function(dat, split_group, sums, purification, ...){
  if (! purification %in% c("none", "2step", "iterative")){
    print("Argument 'purification' is neither 'none', '2step', nor 'iterative'. No purification is done")
    purification <- "none"
  }
  counter <- 0
  remove_NA <- !is.na(as.vector(split_group))

  # remove missings in nodes from MH calculations
  dat <- dat[remove_NA,]
  sums <- sums[remove_NA]
  group <- split_group[remove_NA]

  # calculate MH
  temp <- difR::mantelHaenszel(data = dat, member = group, match = sums)
  temp_MH <- temp$resAlpha
  temp_MH_var <- temp$varLambda
  results_MH <- (2.35) * log(temp_MH)
  results_SD <- (2.35) * sqrt(temp_MH_var)
  which_MH <- which(abs(results_MH) >= 1)
  if ((purification == "2step" | purification == "iterative") & length(which_MH) > 0){
    purified <- purify_MH(dat = dat, group = group, sums = sums, MH_stat = results_MH, which_MH = which_MH)
    results_MH <- purified$results_MH
    results_SD <- purified$results_SD
    counter <- counter + 1
  }
  if (purification == "iterative" & length(which_MH) > 0){
    repeat {
      if(all(which_MH %in% purified$which_MH_purified)){break}
      which_MH <- purified$which_MH_purified
      purified <-  purify_MH(dat = dat, group = group, sums = sums, MH_stat = results_MH, which_MH = which_MH)
      results_MH <- purified$results_MH
      results_SD <- purified$results_SD
      counter <- counter + 1
      if(counter >= ncol(dat)){break}
    }
  }
  MHclassi <- get_ETS_classification(res_MH = data.frame(results_MH), res_MH_sd = data.frame(results_SD))

  MH <- rbind(MH = results_MH,
              SD = results_SD)
  colnames(MH) <- paste("item", 1:ncol(dat), sep = "")
  names(MHclassi) <- paste("item", 1:ncol(dat), sep = "")
  result <- list(mantelhaenszel = MH,
                 classification = c(MHclassi),
                 purification = purification,
                 purification_counter = counter
  )
  return(result)
}
#' Performs purification of the Mantel-Haenszel odds ratio in the Delta-scale
#'
#' @param dat A dataframe with dichotomous item responses
#' @param group An indicator of the two groups for which DIF analyses should be performed
#' @param sums A sum score used as a matching criterion for computing the Mantel Haenszel statistic
#' @param MH_stat A vector or dataframe including the Mantel-Haenszel odds ratio in the Delta-scale
#' @param which_MH a vector containing the item numbers of items with Delta-MH > 1
#'
#' @return A list with purified the Mantel-Haenszel odds ratio in the Delta-scale, standard deviation of purified the Mantel-Haenszel odds ratio in the Delta-scale, a vector     indicating with items were purified
purify_MH <- function(dat, group, sums, MH_stat, which_MH = which_MH){
  # purified MH for no DIF items
  temp_dat <- data.frame(dat[,abs(MH_stat) < 1])
  sums <- rowSums(temp_dat, na.rm = TRUE)
  temp <- difR::mantelHaenszel(data = dat, member = group, match = sums)
  temp_MH <- temp$resAlpha
  temp_MH_var <- temp$varLambda
  results_MH <- (2.35) * log(temp_MH)
  results_SD <- (2.35) * sqrt(temp_MH_var)
  # purified MH for DIF items (including the DIF item in question)
  results_MH[which_MH] <- NA
  for (i in which_MH){
    sumscore <- dat[,i] + sums
    temp_i <- difR::mantelHaenszel(data = data.frame(dat[,i]), member = group, match = sumscore)
    temp_MH_i <- temp_i$resAlpha
    temp_MH_sd_i <- temp_i$varLambda
    results_MH[i] <- (2.35) * log(temp_MH_i)
    results_SD[i] <- (2.35) * sqrt(temp_MH_sd_i)
  }
  which_MH_purified <- which(abs(results_MH) >= 1)
  return(list(results_MH = results_MH,
              results_SD = results_SD,
              which_MH_purified = which_MH_purified))
}
#' Performs classification of the Mantel-Haenszel effect size measured based on ETS guidelines
#' see Dorans & Holland, 1993, p. 41f or Paek & Holland, 2015, Psychometrika
#'
#' @param res_MH A vector or dataframe with the Mantel-Haenszel Odds Ratio effect size measure in the Delta scale (-2.35 * log (Mantel-Haenszel estimate of the common odds ratios)) for each item
#' @param res_MH_sd A vector or dataframe with the standard deviation of Mantel-Haenszel Odds Ratio effect size measure in the Delta scale for each item (-2.35 * sqrt (the variance of the lambda_MH statistic))
#'
#' @return A vector or dataframe with the corresponding A/B/C classification for each item
get_ETS_classification <- function(res_MH, res_MH_sd){
  #' Computes p-values for the statistical hypothesis testing based on log transformation of the Mantel-Haenszel common odds ratio in the Delta-scale for DIF
  #' p values of the hypothesis test are need for the classification of DIF items based on the ETS guidelines
  #' based on Paek & Holland, 2015, Psychometrika
  get_MH_pval <- function(res_MH, res_MH_sd, tau){
    sigtest_mantelhaenszel <- function(MH = 1, sd_MH = .5, tau){
      # schervish <- pnorm((-tau - abs(MH)) / sd_MH) + pnorm((tau - abs(MH)) / sd_MH)
      # return(schervish)
      df <- 1
      lambda <- (tau/sd_MH)^2
      psi <- (MH/sd_MH)^2
      chisqu <- 1 - pchisq(psi, df, lambda)
      return(chisqu)
    }
    p_vals <- data.frame()
    for (i in 1:nrow(res_MH)){
      p_vals <- rbind(p_vals, sigtest_mantelhaenszel(MH = unlist(res_MH[i,]), sd_MH = unlist(res_MH_sd[i,]), tau = tau))
    }
    rownames(p_vals) <- rownames(res_MH); colnames(p_vals) <- colnames(res_MH)
    return(p_vals)
  }
  
  p_vals_A <- get_MH_pval(res_MH, res_MH_sd, tau = 0)
  p_vals_C <- get_MH_pval(res_MH, res_MH_sd, tau = 1)
  MH_classi <- ifelse(abs(res_MH) < 1 | p_vals_A >= .05, "A",
                      ifelse(abs(res_MH) >= 1.5 & p_vals_C < .05, "C",
                             "B"))
  return(MH_classi)
}
