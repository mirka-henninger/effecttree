#' Creates a effecttree object based on the original tree with additional effect size information
#'
#' @param object An object of type modelparty
#' @param purification A character indicating the type of purification ("none", "iterative")
#' @param p.adj A character indicating the correction method for multiple testing. Options are "none", "bonferroni" and "fdr"
#' @param threshold The threshold of partial gamma above which items should be labeled as DIF/DSF items, default is .21 and .31
#' @param reverse_splits A logical indicating whether split should be reversed when the effect size of all items is negligible; default is FALSE
#' @param direction A character either "topdown" or "bottomup" indicating whether stopping (topdown) or pruning (bottomup) should be performed
#' @param evalcrit A character or character vector indicating the evalution criterion. The default is "A", for which all nodes are pruned in which all items are categorized as "A"
#' ############# FIXME ############ add correction for multiple testing
#'
#' @return An object of type modelparty with 'info' extended by a list named effectsize containing entries effect size, classification, purification type, and purificationCounter
#'
#' @examples
#' \dontrun{
#' ## raschtree
#' data("DIFSim", package = "psychotree")
#' RT <- raschtree(resp ~ age + gender + motivation, data = DIFSim)
#' ## add effect size and plot tree
#' RT_MH <- add_effectsize(RT, purification = "iterative")
#' RT_MH$info$effectsize
#' plot(RT_MH, color_by_node = 1)
#'
#' ## use stopping (topdown) based on the effect size (all items in category "A" or "B")
#' ## and plot tree (here no stopping happens)
#' RT_stopped <- add_effectsize(RT, purification = "iterative",
#'                              reverse_splits = TRUE, direction = "topdown", evalcrit = c("A"))
#' RT_stopped$info$effectsize
#' plot(RT_stopped, color_by_node = 2)
#'
#' ## example polytomous items, with/out split-reversal:
#' data("dat_organization")
#' items <- as.data.frame(dat_organization[, grep("item", names(dat_organization))])
#' dat_organization$items <- apply(items,2,as.numeric)
#' pc_tree <- pctree(items ~ sex + age + length_service + same_position + leadership,
#'                   data = dat_organization)
#' plot(pc_tree)
#' pc_tree_pgamma <- add_effectsize(pc_tree, purification = "iterative", p.adj = "fdr")
#' pc_tree_pgamma_stopped <- add_effectsize(pc_tree, purification = "iterative", p.adj = "fdr",
#'                                          reverse_splits = TRUE, direction = "topdown")
#' plot(pc_tree_pgamma, color_by_node = 1)
#' plot(pc_tree_pgamma, type = "regions", color_by_node = 1)
#' plot(pc_tree_pgamma_stopped, color_by_node = 1)
#' plot(pc_tree_pgamma_stopped, type = "regions", color_by_node = 1)
#' }
#' @export
add_effectsize <- function(object, purification, p.adj, threshold = c(.21, .31), reverse_splits = FALSE, direction = c("topdown", "bottomup"), evalcrit = "A"){
  # check whether object is of type modelparty, and party
  if(!(any(class(object) %in% c("modelparty", "party"))))
    stop("Object must be a modelparty object (as returned from the raschtree or pctree function")
  model <- class(object)[1]
  object$info$effectsize <- get_effectsize(object, model = model, purification = purification, p.adj = p.adj)

  # stopping/pruning function
  if(isTRUE(reverse_splits)){
    which_nodes_pruned <- get_prune_nodes(object, direction = direction, evalcrit = evalcrit)
    if(length(which_nodes_pruned > 0 )){
      pruned_tree <- nodeprune(object, ids = which_nodes_pruned)
      object <- add_effectsize(pruned_tree, purification = purification,  p.adj = p.adj)
    }
  }
  class(object) <- c("effecttree", class(object))
  return(object)
}
#' Plots the effect size tree
#'
#' @description Additional options to show the effect size classification in the inner nodes,
#' option to color the items in terminal nodes by A/B/C DIF classification based on inner nodes,
#' and option to turn on a background color so you can see which are the left and right terminal nodes for the inner nodes which items are colored
#'
#' @param x An object of type effecttree with 'info' extended by a list named effect size, classification, purification type, and purificationCounter
#' @param type character specifying the type of predictions or plot ("profile" for item difficulties/locations, "regions" for regions of expected item responses)
#' @param show_classification A logical with default is TRUE: Should the classification be shown in each inner node?
#' @param color_by_node An integer indicating the inner node after which the item parameter should be colored
#' @param ABC_colors A character vector of length three indicating the colors in which items classified as A/B/C should be displayed
#' @param ABC_size A vector of length three indicating the size in which items classified as A/B/C should be displayed
#' @param node_background A character vector of length two indicating the background colors of the panels for which the items have been compared
#' @param ... arguments passed to the underlying functions, i.e., to mob_control for raschtree, and to the underlying predict and plot methods, respectively.
#' @return An object of S3 class "effecttree" inheriting from class "modelparty".
#'
#' @export
plot.effecttree <- function(x,
                            type = "profile",
                            show_classification = TRUE,
                            color_by_node = NULL,
                            ABC_colors = c("#99e1e3", "#ba6100", "#6c0200"),
                            ABC_size = c(.4,.75,.9),
                            node_background = c("#eee3af", "#aeaeae"), ...){
  # define inner and terminal panels
  tp_args <- list(...)
  inner_panel <- node_inner
  if(show_classification == TRUE) inner_panel <- show_effectsize

  if(type == "profile"){
    terminal_panel <- node_profileplot
    if(!is.null(color_by_node)) terminal_panel <- color_by_node(node_ID = color_by_node,
                                                                class_color = ABC_colors,
                                                                class_size = ABC_size,
                                                                panel_color = node_background)
  }
  if(type == "regions"){
    terminal_panel <- node_regionplot
    if(!is.null(color_by_node)){
      tp_args[['node_col_fun']] = color_by_node_regions(x, color_by_node)
    }
  }
  # plot effecttree based on the original raschtree
  partykit::plot.modelparty(x,
                            terminal_panel = terminal_panel,
                            inner_panel = inner_panel,
                            tp_args = tp_args)
}
