#' Extract the inner nodes to be pruned from the effect size measure
#'
#' @param object An object of type effecttree
#' @param direction A character either "topdown" or "bottomup" indicating whether stopping (topdown) or pruning (bottomup) should be performed
#' @param evalcrit A character or character vector indicating the evalution criterion. The default is "A", for which all nodes are pruned in which all items are categorized as "A"
#'
#' @return A vector with the number of the nodes to be pruned
get_prune_nodes <- function(object, direction, evalcrit){
  prune_nodes <- apply(object$info$effectsize$classification,2,function(x){all(x %in% evalcrit)})
  prune_list <- as.list(prune_nodes)
  names(prune_list) <- sub("node", "", names(prune_nodes))

  # this list is already suitable for topdown stopping
  # for bottom-up pruning, an additional function is necessary
  prune_from_bottom <- function(nodes, prune_list){
    children <- kids_node(nodes)
    if(is.null(children)){
      return(list())
    }
    lsname <- as.character(nodes$id)
    left <- children[[1]]
    right <- children[[2]]
    left_id <- as.character(left$id)
    right_id <- as.character(right$id)
    prune_list_left <- prune_from_bottom(left, prune_list)
    prune_list_right <- prune_from_bottom(right, prune_list)
    prune_list_bottom <- c(prune_list_right, prune_list_left)
    # check if the node is
    # (a) a terminal node (--> does not prevent pruning) or
    # (b) should be pruned
    prune_left <- is.null(prune_list_bottom[[left_id]]) | prune_list_bottom[[left_id]]
    prune_right <- is.null(prune_list_bottom[[right_id]]) | prune_list_bottom[[right_id]]
    # should the node we are currently visiting be pruned?
    prune_self <- prune_list[[lsname]]
    prune_list_bottom[[lsname]] <- all(prune_self, prune_left, prune_right)
    return(prune_list_bottom)
  }

  if(direction == "bottomup"){
    prune_list <- prune_from_bottom(object$node, prune_list)
  }
  prune_nodes_list <- names(which(unlist(prune_list), useNames = TRUE))
  return(prune_nodes_list)
}
