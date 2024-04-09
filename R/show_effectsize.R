#' Helper function to display the effect size measure and classification summary
#' in each inner node of the tree
#' @description  This function is largely copied from partykit:::node_inner and
#' adapted to display larger inner nodes with corresponding effect size classification summaries
#'
#' @param object An object of class modelparty that has the effect size measure added to it. See @examples
#' @param id Argument copied from partykit:::node_inner
#' @param pval Argument copied from partykit:::node_inner
#' @param abbreviate Argument copied from partykit:::node_inner
#' @param fill Argument copied from partykit:::node_inner
#' @param gp Argument copied from partykit:::node_inner
#'
#' @return A function that can be used as a value for the argument 'inner_panel' in plot.raschtree() or plot.pctree()
show_effectsize <- function(object, id = TRUE, pval = TRUE, abbreviate = FALSE,
                            fill = "white", gp = gpar())
{
  # check whether the effect size is saved in the tree object
  if(is.null(object$info$effectsize))
    stop("No effect size classification found. Please use the add_effectsize function to add effect size measures to the tree object")
  # extract effect size
  classi <- data.frame(object$info$effectsize$classification)
  classi <- lapply(classi, function(x) factor(x, levels = c("A", "B", "C")))
  table_classi <- sapply(classi, table)
  tab_classi <- apply(table_classi, 2, function(x) paste(rownames(table_classi), ":", x, "; ", sep = ""))
  tab_classi <- apply(tab_classi, 2, function(x) paste(c(rep("_", 14), "\neffect size: ", x), collapse = ""))
  message(paste("Purification of effect size measure:", object$info$effectsize$purification[1], sep = " "))

  # original node_inner function
  meta <- object$data
  nam <- names(object)

  extract_label <- function(node) {
    if(is.terminal(node))
      return(rep.int("", 2L))
    varlab <- partykit::character_split(split_node(node), meta)$name
    if(abbreviate > 0L)
      varlab <- abbreviate(varlab, as.integer(abbreviate))
    if(pval) {
      nullna <- function(x) is.null(x) || is.na(x)
      pval <- suppressWarnings(try(!nullna(info_node(node)$p.value),
                                   silent = TRUE))
      pval <- if(inherits(pval, "try-error")) FALSE
      else pval
    }
    if(pval) {
      pvalue <- node$info$p.value
      plab <- ifelse(pvalue < 10^(-3L), paste("p <", 10^(-3L)),
                     paste("p =", round(pvalue, digits = 3L)))
    } else {
      plab <- ""
    }
    tab_classi <- tab_classi[[paste("node", nam[id_node(node)], sep = "")]]
    return(c(varlab, plab, tab_classi))
  }

  maxstr <- function(node) {
    lab <- extract_label(node)
    klab <- if(is.terminal(node)) ""
    else unlist(lapply(kids_node(node), maxstr))
    lab <- c(lab, klab)
    lab <- unlist(lapply(lab, function(x) strsplit(x, "\n")))
    lab <- lab[which.max(nchar(lab))]
    if(length(lab) < 1L) lab <- ""
    return(lab)
  }

  nstr <- maxstr(node_party(object))
  if(nchar(nstr) < 6) nstr <- "aAAAAa"

  ### panel function for the inner nodes
  rval <- function(node) {
    pushViewport(viewport(gp = gp, name = paste("node_inner",
                                                id_node(node), "_gpar", sep = "")))
    node_vp <- viewport(x = unit(0.5, "npc"), y = unit(0.5,
                                                       "npc"), width = unit(1, "strwidth", nstr) * 1.3,
                        height = unit(5, "lines"), name = paste("node_inner",
                                                                id_node(node), sep = ""), gp = gp)
    pushViewport(node_vp)

    xell <- c(seq(0, 0.2, by = 0.01),
              seq(0.2, 0.8, by = 0.05),
              seq(0.8, 1, by = 0.01))
    yell <- sqrt(xell * (1-xell))

    lab <- extract_label(node)
    fill <- rep(fill, length.out = 2L)

    grid.polygon(x = unit(c(xell, rev(xell)), "npc"),
                 y = unit(c(yell, -yell)+0.5, "npc"),
                 gp = gpar(fill = fill[1]))

    ## FIXME: something more general instead of pval ?
    grid.text(lab[1L], y = unit(3 + 0.5 * (lab[2L] != "") + 0.5 * (lab[3L] != ""), "lines"))
    if(lab[2L] != "") grid.text(lab[2L], y = unit(2.5 + 0.5 * (lab[3L] != ""), "lines"))
    if(lab[3L] != "") grid.text(lab[3L], y = unit(2, "lines"))

    if(id) {
      nodeIDvp <- viewport(x = unit(0.5, "npc"), y = unit(1, "npc"),
                           width = max(unit(1, "lines"), unit(1.3, "strwidth", nam[id_node(node)])),
                           height = max(unit(1, "lines"), unit(1.3, "strheight", nam[id_node(node)])))
      pushViewport(nodeIDvp)
      grid.rect(gp = gpar(fill = fill[2]))
      grid.text(nam[id_node(node)])
      popViewport()
    }
    upViewport(2)
  }
  return(rval)
}
class(show_effectsize) <- "grapcon_generator"
