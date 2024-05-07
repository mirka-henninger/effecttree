#' Calculates the partial gamma coefficient for DIF/DSF in polytomous items
#'
#' @param dat A dataframe with polytomous item responses
#' @param split_group An indicator of the two groups for which DIF/DSF analyses should be performed
#' @param purification A character indicating the type of purification that is used on the partial gamma coefficient. Options are "none", "2step", or "iterative"
#' @param p.adj A character indicating the correction method for multiple testing. Options are "none", "bonferroni" and "fdr"
#' @param threshold The threshold of partial gamma above which items should be labeled as DIF/DSF items, default is .21 and .31
#' @param ... Further arguments
#'
#' @return A list with the partial gamma coefficients, the classification (A/B/C), the information on the correction method for multiple testing, the information on purification and convergence of purification if iterative purification was done.
calculate_pgamma <- function(dat, split_group, purification, p.adj, threshold = c(.21, .31), ...){
  if (! purification %in% c("none", "2step", "iterative")){
    print("Argument 'purification' is neither 'none', '2step' nor 'iterative'. No purification is done")
    purification <- "none"
  }

  counter <- 0
  remove_NA <- !is.na(as.vector(split_group))

  ## remove missings in nodes from pgamma calculations
  dat <- dat[remove_NA,]
  group <- split_group[remove_NA]

  ## calculate pgamma
  res_gamma <- quiet(iarm::partgam_DIF(dat.items = dat, dat.exo = group))
  which_gamma <- which(abs(res_gamma$gamma)>= threshold[1])
  results_pgamma <- res_gamma$gamma
  results_se <- res_gamma$se

  ## 2step purification
  if ((purification == "2step" | purification == "iterative") & length(which_gamma) > 0){
    purified <- purify_pgamma(dat = dat, group = group, res_gamma = res_gamma, threshold = threshold[1])
    counter <- counter + 1
  }

  ## iterative purification
  if (purification == "iterative" & length(which_gamma) > 0){
    which_gamma_excluded <- NA
    repeat {
      purified <- purify_pgamma(dat = dat, group = group, res_gamma = purified, threshold = threshold[1])
      counter <- counter + 1
      which_gamma_excluded <- which(abs(purified$gamma) >= threshold[1])
      results_pgamma <- purified$gamma
      results_se <- purified$se
      if(all(which_gamma %in% which_gamma_excluded)){break}
      if(counter >= ncol(dat)){break}
      which_gamma <- which_gamma_excluded
    }
  }

  ## item classification
  if(!all(is.numeric(results_pgamma))){
    warning("Classification is not possible.")
    pgamma_classi <- rep(NA, length(results_pgamma))
  }
  pgamma_classi <- try(get_ABC_classification(res_pgamma = results_pgamma, res_pgamma_se = results_se, p.adj = p.adj, threshold))

  pgamma <- rbind(pgamma = results_pgamma,
                  se = results_se)
  colnames(pgamma) <- paste("item", 1:ncol(dat), sep = "")
  names(pgamma_classi) <- paste("item", 1:ncol(dat), sep = "")
  result <- list(pgamma = pgamma,
                 classification = c(pgamma_classi),
                 p.adj = p.adj,
                 purification = purification,
                 purification_counter = counter
  )
  return(result)
}
#' Performs purification of the partial gamma coefficient
#'
#' @param dat A dataframe with polytomous item responses
#' @param group An indicator of the two groups for which DIF/DSF analyses should be performed
#' @param res_gamma A vector with partial gamma coefficient
#' @param threshold The threshold of partial gamma above which items should be labeled as DIF items
#'
#' @return A dataframe including purified partial gamma coefficients
purify_pgamma <- function(dat, group, res_gamma, threshold){
  purified_gamma <- res_gamma
  which_noDIF <- which(abs(res_gamma$gamma) < threshold[1])
  which_DIF <- which(abs(res_gamma$gamma) >= threshold[1])
  if(length(which_noDIF) <= 1){
    stop("Purification is not possible, too many items with DIF/DSF.")
  }
  temp_dat <- dat[,which_noDIF, drop = FALSE]

  # no DIF items
  temp <- quiet(iarm::partgam_DIF(temp_dat,group))
  temp$Item <- paste0("I", which_noDIF)
  purified_gamma[which_noDIF,] <- temp

  # DIF items
  if(length(which_DIF) > 0){
    for(i in which_DIF){
      which_noDIF <- sort(c(i, which_noDIF))
      temp_dat <- dat[,which_noDIF, drop = FALSE]
      temp <- quiet(iarm::partgam_DIF(temp_dat,group))
      purified_gamma[i,] <- temp[i,]
    }
  }
  return(purified_gamma)
}

#' Performs classification of the partial gamma effect size based on Bjorner et al. (1998)
#'
#' @param res_pgamma A vector or dataframe with the partial gamma coefficient for each item
#' @param res_pgamma_se A vector or dataframe with the standard error of partial gamma coefficient for each item
#' @param p.adj A character indicating the correction method for multiple testing. Options are "none", "bonferroni" and "fdr"
#' @param threshold The threshold of partial gamma above which items should be labeled as DIF/DSF items, default is .21 and .31
#'
#' @return A vector or dataframe with the corresponding A/B/C classification for each item
get_ABC_classification <- function(res_pgamma, res_pgamma_se, p.adj, threshold){

  # get p-values
  get_pgamma_pval <- function(res_pgamma, res_pgamma_se, p.adj, threshold, ...){

    # test for A classification
    p_vals_A <- ifelse(res_pgamma > 0, 2 * (1 - pnorm(res_pgamma / res_pgamma_se)), 2 * (pnorm(res_pgamma / res_pgamma_se)))

    # test for C classification
    pgamma_shifted <- ifelse(res_pgamma >= 0, res_pgamma - threshold[1], res_pgamma + threshold[1])

    p_vals_C <- ifelse(pgamma_shifted > 0, 2 * (1 - pnorm(pgamma_shifted / res_pgamma_se)), 2 * (pnorm(pgamma_shifted / res_pgamma_se)))

    if(p.adj != "none"){
      p_vals_A <- p.adjust(p_vals_A, method= p.adj, n= length(res_pgamma))
      p_vals_C <- p.adjust(p_vals_C, method= p.adj, n= length(res_pgamma))
    }

    p_vals <- data.frame(A = p_vals_A, C = p_vals_C)

    return(p_vals)
  }

  p_vals <- get_pgamma_pval(res_pgamma, res_pgamma_se, p.adj, threshold)

  # DIF/DSF classification
  pgamma_classi <- ifelse(abs(res_pgamma) < threshold[1] | p_vals$A > 0.05, "A", ifelse(abs(res_pgamma) > threshold[2] & p_vals$C < 0.05, "C", "B"))

  return(pgamma_classi)
}
