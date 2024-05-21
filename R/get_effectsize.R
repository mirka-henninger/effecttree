#' Function that returns the effect size measure of a tree object
#'
#' @param object An object of type modelparty
#' @param model A character indicating the model in the tree object ("raschtree", "pctree")
#' @param purification A character indicating the type of purification ("none", "2step", "iterative")
#' @param p.adj A character indicating the correction method for multiple testing. Options are "none", "bonferroni" and "fdr"
#' @param threshold The threshold of partial gamma above which items should be labeled as DIF/DSF items, default is .21 and .31
#'
#' @return A list with entries effect size, classification, purification type, and purification_counter for each inner node
#'
#' @examples
#' \dontrun{
#' ## raschtree
#' data("DIFSim", package = "psychotree")
#' RT <- raschtree(resp ~ age + gender + motivation, data = DIFSim)
#' eff <- get_effectsize(RT, model = "raschtree", purification = "iterative")
#' eff
#' }
#'
#' @export
get_effectsize <- function(object, model, purification, p.adj, threshold = c(.21, .31)){
  # check whether object is of type modelparty, and party
  if(!(any(class(object) %in% c("modelparty", "party"))) &
     model %in% class(object))
    stop("Object must be an model object (as returned from the raschtree or pctree function")

  # extract information from tree
  ids <- which(!(nodeids(object, terminal = FALSE) %in% nodeids(object, terminal = TRUE)))
  dat_fitted <- data_party(object) # dataframe with data, split variables and fitted groups (end nodes)
  splits <- node_party(object) # where the splits are

  # prepare data for effect size calculation
  split_groups <- return_split_groups(node = splits, dat_fitted)
  dat <- dat_fitted[[1]]
  sums <- rowSums(dat)
  node_names <- paste("node", ids, sep = "")

  # distinguish raschtree and pctree
  if(model == "raschtree"){
    MH <- lapply(split_groups, function(grp)(calculate_mantelhaenszel(dat = dat, split_group = grp, sums = sums, purification = purification)))
    summary_mantelhaenszel <- function(x){
      list(classification = sapply(x, function(x) x$classification),
           mantelhaenszel = sapply(x, function(x) x$mantelhaenszel[1,]),
           purification =  sapply(x, function(x) x$purification),
           purification_counter = sapply(x, function(x) x$purification_counter)
      )
    }
    effectsize <- summary_mantelhaenszel(MH)
  }
  if(model == "pctree"){
    pgamma <- lapply(split_groups, function(grp)(calculate_pgamma(dat = dat, split_group = grp, purification = purification, p.adj = p.adj, threshold = threshold)))
    summary_pgamma <- function(x){
      list(classification = sapply(x, function(x) x$classification),
           pgamma = sapply(x, function(x) x$pgamma[1,]),
           p.adj =  sapply(x, function(x) x$p.adj),
           purification =  sapply(x, function(x) x$purification),
           purification_counter = sapply(x, function(x) x$purification_counter)
      )
    }
    effectsize <- summary_pgamma(pgamma)
  }
  return(effectsize)
}
