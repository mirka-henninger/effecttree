#' Function that returns the effect size measure of a tree object
#'
#' @param object An object of type modelparty
#' @param type A character indicating the type of the tree object ("raschtree", "pctree")
#' @param purification A character indicating the type of purification ("none", "iterative")
#'
#' @return A list with entries effect size, classification, purification type, and purification_counter for each inner node
#'
#' @examples
#' \dontrun{
#' data("DIFSim", package = "psychotree")
#' RT <- raschtree(resp ~ age + gender + motivation, data = DIFSim)
#' eff <- get_effectsize(RT, type = "raschtree", purification = "iterative")
#' eff
#' }
#'
#' @export
get_effectsize <- function(object, type, purification){
  ####### FIXME add argument multiple-testing correction
  ####### FIXME add match.arg for type (raschtree / pctree)

  # check whether object is of type modelparty, and party
  if(!(any(class(object) %in% c("modelparty", "party"))) &
     type %in% class(object))
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

  ######### FIXME ######### if-statement to distinguish between delta-MH and partial gamma (something more elegant?)
  if(type == "raschtree"){
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
  if(type == "pctree"){
    ############ FIXME add partial gamma here
  }
  return(effectsize)
}
