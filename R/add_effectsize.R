#' Creates a effecttree object based on the original tree with additional effect size information
#'
#' @param object An object of type modelparty
#' @param type A character indicating the type of the tree ("raschtree", "pctree")
#' @param purification A character indicating the type of purification ("none", "iterative")
#' ############# FIXME ############ add correction for multiple testing
#'
#' @return An object of type modelparty with 'info' extended by a list named effectsize containing entries effect size, classification, purification type, and purificationCounter
#'
#' @examples
#' \dontrun{
#' data("DIFSim", package = "psychotree")
#' RT <- raschtree(resp ~ age + gender + motivation, data = DIFSim)
#' RT_MH <- add_effectsize(RT, type = "raschtree", purification = "iterative")
#' RT_MH$info$effectsize
#' plot(RT_MH, color_by_node = 1)
#' }
#' @export
add_effectsize <- function(object, type, purification){
  # check whether object is of type modelparty, and party
  if(!(any(class(object) %in% c("modelparty", "party"))) &
     type %in% class(object))
    stop("Object must be a modelparty object (as returned from the raschtree or pctree function")
  object$info$effectsize <- get_effectsize(object, type = type, purification = purification, by = "type")
  class(object) <- c("effecttree", class(object))
  return(object)
}
############ FIXME ############
############ add stopping / pruning function
#' Plots the effect size tree
#'
#' @description Additional options to show the effect size classification in the inner nodes,
#' option to color the items in terminal nodes by A/B/C DIF classification based on inner nodes,
#' and option to turn on a background color so you can see which are the left and right terminal nodes for the inner nodes which items are colored
#'
#' @param x An object of type effecttree with 'info' extended by a list named effect size, classification, purification type, and purificationCounter
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
                            show_classification = TRUE,
                            color_by_node = NULL,
                            ABC_colors = c("#99e1e3", "#ba6100", "#6c0200"),
                            ABC_size = c(.4,.75,.9),
                            node_background = c("#eee3af", "#aeaeae"), ...){

  # define inner and terminal panels
  inner_panel <- node_inner
  if(show_classification == TRUE) inner_panel <- show_effectsize

  terminal_panel <- node_profileplot
  if(!is.null(color_by_node)) terminal_panel <- color_by_node(node_ID = color_by_node,
                                                              class_color = ABC_colors,
                                                              class_size = ABC_size,
                                                              panel_color = node_background)

  # plot raschtreeMH based on the original raschtree
  partykit::plot.modelparty(x,
                            terminal_panel = terminal_panel,
                            inner_panel = inner_panel, ...)
}
