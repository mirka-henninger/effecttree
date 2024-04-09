############## FIXME ############ later: how are the different end nodes colored for the partial gamma coefficient? Profile plot as MHtree, regionplot differently
############## include both versions in this file

#' Helper function to to be used for the 'col' argument in the plot method for Raschtrees
#' Helps to color item parameter in profileplots by inner nodes and ETS Mantel-Haenszel classification
#'
#' @param node_ID An integer indicating the inner node after which the item paramter should be colored
#' @param class_color A character vector indicating the colors for items classified as A, B, or C
#' @param class_size A character vector indicating the size for items classified as A, B, or C
#' @param panel_color A character vector indicating the colors of the background panels for left and right terminal nodes of the inner node by which items are colored
#'
#' @return A function that takes the argument 'purification'node_ID' and 'color' and can be used as a value for the argument 'terminal_panel' in plot.raschtree()
color_by_node <- function(node_ID, class_color, class_size, panel_color) {
  return_color_fun <- function(object, ...){
    return_fun <-
      psychotree::node_profileplot(object,
                                   col = create_color_list(object, node_ID = node_ID, pars = class_color, default = "black"),
                                   cex = create_color_list(object, node_ID = node_ID, pars = class_size, default = .4),
                                   border = "black",
                                   bg = create_bg_list(object, node_ID = node_ID, background_cols = panel_color),
                                   cf_fun = anchor_pars(node_ID))
    return(return_fun)
  }
  message(paste("Colored by Node", node_ID))
  class(return_color_fun) <- "grapcon_generator"
  return(return_color_fun)
}
#' Helper function to create a list with colors and point sizes for each terminal node
#'
#' @param object An object of class modelparty that has an effect size measure added to it.
#' @param node_ID An integer indicating the inner node after which the item parameter should be colored
#' @param pars A character vector of length three indicating the colors in which items classified as A/B/C should be displayed
#' @param default The default color for end nodes that are not colored
#'
#' @return A list named after the terminal nodes containing the colors of the item parameter based on ETS Mantel-Haenszel classification
create_color_list <- function(object, node_ID, pars, default){
  # check whether effect size is saved in the Raschtree object
  if(is.null(object$info$effectsize))
    stop("No effect size classification found. Please use the add_effectsize function to add an effect size measures to the tree object")

  # get IDs of inner nodes
  nodes <- node_party(object) # where the splits are
  inner_nodes <- get_inner_nodes(nodes)
  inner_nodes_IDs <- sapply(inner_nodes, function(x) x$id)

  # check whether node_ID describes an inner node
  if(!(node_ID %in% inner_nodes_IDs)) stop(c("Coloring can only be executed based on effect size classification in inner nodes (inner nodes: ", paste(as.character(inner_nodes_IDs), collapse = ", "), ")"))

  # get terminal nodes of node_ID and the remaining node IDs
  which_ID <- which(inner_nodes_IDs == node_ID)
  terminal_nodes <- get_terminal_nodes(inner_nodes[[which_ID]])
  allterminal_nodes <- get_terminal_nodes(nodes)
  otherterminal_nodes <- allterminal_nodes[!allterminal_nodes %in% terminal_nodes]

  # create the color list
  eff_classi <- object$info$effectsize$classification
  color_info <- data.frame(eff_classi)[,which(colnames(eff_classi) == paste("node", node_ID, sep = ""))]
  color_fun <- function(x){ifelse(x == "A", pars[1], ifelse(x == "B", pars[2], ifelse(x == "C", pars[3], x)))}
  color_by <- color_fun(color_info)

  color_list <- rep(list(list()), length(allterminal_nodes))
  color_list <- stats::setNames(color_list, as.character(allterminal_nodes))
  for(entry in terminal_nodes){
    color_list[[which(names(color_list) == entry)]] <- color_by
  }
  for(entry in otherterminal_nodes){
    color_list[[which(names(color_list) == entry)]] <- default
  }
  return(color_list)
}
#' Helper function to create a list with colors for left and right child node of the node by which A/B/C coloring is performed
#'
#' @param object An object of class modelparty that has effect size measure added to it.
#' @param node_ID An integer indicating the inner node after which the item parameter should be colored
#' @param background_cols A character vector indicating the colors of the background panels for left and right terminal nodes of the inner node by which items are colored
#'
#' @return A list named after the terminal nodes containing the colors of the item parameter based on ETS Mantel-Haenszel classification
create_bg_list <- function(object, node_ID, background_cols){
  # check whether an effect size is saved in the Raschtree object
  if(is.null(object$info$effectsize))
    stop("No Mantel-Haenszel classification found. Please use the add_mantelhaenszel function to add Mantel-Haenszel effect size measures to the Raschtree object")

  # get IDs of inner nodes
  nodes <- node_party(object) # where the splits are
  inner_nodes <- get_inner_nodes(nodes)
  inner_nodes_IDs <- sapply(inner_nodes, function(x) x$id)

  # check whether node_ID describes an inner node
  if(!(node_ID %in% inner_nodes_IDs)) stop(c("Coloring can only be executed based on an effect size classification in inner nodes (inner nodes: ", paste(as.character(inner_nodes_IDs), collapse = ", "), ")"))

  # get terminal nodes of node_ID and the remaining node IDs
  which_ID <- which(inner_nodes_IDs == node_ID)
  children_left <- kids_node(inner_nodes[[which_ID]])[[1]]
  children_right <- kids_node(inner_nodes[[which_ID]])[[2]]
  terminal_left <- get_terminal_nodes(children_left)
  terminal_right <- get_terminal_nodes(children_right)

  bg_list <- rep(list(list()), length(c(terminal_left, terminal_right)))
  bg_list <- stats::setNames(bg_list, as.character(c(terminal_left, terminal_right)))
  for(entry in terminal_left){
    bg_list[[which(names(bg_list) == entry)]] <- background_cols[1]
  }
  for(entry in terminal_right){
    bg_list[[which(names(bg_list) == entry)]] <- background_cols[2]
  }
  return(bg_list)
}
#' Helper function for anchoring items in the Rasch tree profile plot
#'
#' @param node_ID An integer indicating the inner node after which the item paramter should be colored
#'
#' @return function cf that will be used to extract anchored item parameter from the mob object
anchor_pars <- function(node_ID){
  return(function(object, nodes){
    # get IDs of inner nodes
    inner_nodes <- get_inner_nodes(node_party(object))
    inner_nodes_IDs <- sapply(inner_nodes, function(x) x$id)

    # get terminal nodes of node_ID and the remaining node IDs
    which_ID <- which(inner_nodes_IDs == node_ID)
    anchoring_nodes <- get_terminal_nodes(inner_nodes[[which_ID]])
    other_nodes <- nodes[!nodes %in% anchoring_nodes]

    # use only no-DIF items in a specific node for anchoring
    eff_classi <- object$info$effectsize$classification
    anchor_info <- data.frame(eff_classi)[,which(colnames(eff_classi) == paste("node", node_ID, sep = ""))]
    anchor_items <- which(anchor_info == "A")
    message("Anchor-Items:"); message(paste(anchor_items, "- "))
    # only terminal nodes of the inner node are anchored, other nodes use sum-to-zero
    unchanged <- apply_to_models(object, other_nodes, FUN = function(x) coef(itempar(x, alias = TRUE, vcov = FALSE)))
    anchored <- apply_to_models(object, anchoring_nodes, FUN = function(x) coef(itempar(x, ref = anchor_items, alias = TRUE, vcov = FALSE)))
    cf <- as.list(c(unchanged, anchored))
    cf <- cf[order(names(cf))]
    return(cf)
  })
}
# extracted from partykit
apply_to_models <- function (object, node = NULL, FUN = NULL, drop = FALSE, ...)
{
  if (is.null(node))
    node <- nodeids(object, terminal = FALSE)
  if (is.null(FUN))
    FUN <- function(object, ...) object
  rval <- if ("object" %in% object$info$control$terminal) {
    nodeapply(object, node, function(n) FUN(info_node(n)$object))
  }
  else {
    lapply(partykit::refit.modelparty(object, node, drop = FALSE),
           FUN)
  }
  names(rval) <- node
  if (drop & length(node) == 1L)
    rval <- rval[[1L]]
  return(rval)
}
