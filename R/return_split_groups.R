#' Returns the split groups of a Raschtree based on the node structure and the allocation of respondents in terminal nodes
#' @param node A node in a Raschtree
#' @param dat_fitted A dataframe with data, split variables and fitted groups (end nodes) as returned from partykit::data_party(object)
#'
#' @return Split groups of a node in a Raschtree
return_split_groups <- function(node, dat_fitted){
  inner_nodes <- get_inner_nodes(node)
  get_id <- function(x){
    ids <- x$id
    res <- paste("node", ids, sep = "")
    return(res)
  }

  #' Get the split groups for a node in a tree object
  get_split_group <- function(node, dat_fitted){
    if(is_terminal_node(node) == TRUE){
      errorCondition("not possible")
    }
    children <- kids_node(node)
    left <- children[[1]]
    right <- children[[2]]
    terminal_left <- get_terminal_nodes(left)
    terminal_right <- get_terminal_nodes(right)

    map_it <- function(x){
      if(x %in% terminal_left){return(0)}
      if(x %in% terminal_right){return(1)}
      return(NA)
    }
    res <- unlist(lapply(dat_fitted$`(fitted)`, map_it))

    return(res)
  }

  split_groups <- lapply(inner_nodes, get_split_group, dat_fitted)
  names(split_groups) <- lapply(inner_nodes, get_id)
  return(split_groups)
}
#' Returns the terminal nodes of a tree object
#' @param node A node in a Raschtree
#'
#' @return terminal nodes of a tree object
get_terminal_nodes <- function(node){
  if(is_terminal_node(node) == TRUE){
    id <- node$id
    res <- list(id = id)
  }
  if(is_terminal_node(node) == FALSE){
    children <- kids_node(node)
    res <- list()
    for (i in 1:(length(children))){
      res[[i]] <- get_terminal_nodes(children[[i]])
    }
    res <- unlist(res)
  }
  return(res)
}
#' Function that returns the inner nodes of a tree object
#' @param node A node in a Raschtree
#'
#' @return inner nodes of a tree object
get_inner_nodes <- function(node){
  if(is_terminal_node(node)){
    res <- list()
  }
  if(is_terminal_node(node) == FALSE){
    children <- kids_node(node)
    res <- list()
    for (i in 1:(length(children))){
      res[[i]] <- get_inner_nodes(children[[i]])
    }
    res[[length(children)+1]] <- list(node = node)
    res <- unlist(res, recursive=FALSE)
  }
  return(res)
}
#' checks whether a node is a terminal node
#' @param node A node in a Raschtree
is_terminal_node <- function(node){
  res <- is.null(partykit::kids_node(node))
  return(res)
}
