node_profileplot <- function(mobobj, what = c("items", "thresholds", "discriminations"),
                             parg = list(type = NULL, ref = NULL, alias = TRUE), cf_fun = NULL, id = TRUE, names = FALSE,
                             abbreviate = TRUE, index = TRUE, ref = TRUE, col = "black", border = col,
                             linecol = "black", refcol = "lightgray", bg = "white", cex = 0.5, pch = 21,
                             xscale = NULL, yscale = NULL, ylines = 2, ...)
{
  ## check input
  what <- match.arg(what)
  if (what == "thresholds") type <- parg$type
  refpar <- parg$ref
  alias <- if (is.null(parg$alias)) TRUE else parg$alias
  addargs <- list(...)
  if ("worth" %in% names(addargs)) warning("the argument 'worth' is deprecated and not longer used")

  ## node ids
  node <- nodeids(mobobj, terminal = FALSE)

  ## get all coefficients
  if (is.function(cf_fun)) {
    cf <- cf_fun(mobobj, node)
  } else if (what == "items") {
    cf <- apply_to_models(mobobj, node, FUN = function(z) coef(itempar(z, ref = refpar, alias = alias, vcov = FALSE)))
  } else if (what == "thresholds") {
    cf <- apply_to_models(mobobj, node, FUN = function(z) coef(threshpar(z, type = type, ref = refpar, alias = alias, vcov = FALSE), type = "matrix"))
  } else {
    cf <- apply_to_models(mobobj, node, FUN = function(z) coef(discrpar(z, ref = refpar, alias = alias, vcov = FALSE)))
  }
  names(cf) <- node

  ## labeling
  if (isTRUE(names)) {
    nms <- if (what != "thresholds") lapply(cf, names) else lapply(cf, rownames)
  } else if (is.character(names)) {
    nms <- split(rep(names, length(node)), f = rep(1:length(node), each = length(names)))
  } else {
    ncf <- lapply(cf, NROW)
    nms <- lapply(ncf, function(m) {
      lab <- rep("", m)
      lab[c(1, m)] <- c(1, m)
      pr <- pretty(1:m, n = 4)
      pr <- pr[pr > 1 & pr < m]
      lab[pr] <- pr
      lab
    })
    abbreviate <- FALSE
  }

  ## abbreviation
  if (is.logical(abbreviate)) {
    nlab <- max(unlist(lapply(nms, function (j) nchar(j))))
    abbreviate <- if (abbreviate) as.numeric(cut(nlab, c(-Inf, 1.5, 4.5, 7.5, Inf))) else nlab
  }
  nms <- lapply(nms, function (j) abbreviate(j, abbreviate))

  ## axis scale
  if (index) {
    x <- if (what == "thresholds") 1:nrow(cf[[1]]) else 1:length(cf[[1]])
    if (is.null(xscale)) xscale <- range(x) + c(-0.1, 0.1) * diff(range(x))
  } else {
    if (what == "thresholds") {
      x <- 0:(ncol(cf[[1]]) - 1)
      if (is.null(xscale)) xscale <- c(-1, ncol(cf[[1]]))
    } else {
      x <- rep(0, length(cf[[1]]))
      if (is.null(xscale)) xscale <- c(-1, 1)
    }
  }
  rg <- range(unlist(cf)[is.finite(unlist(cf))], na.rm = TRUE)
  r <- diff(rg)
  if (!r) r <- 1
  if (is.null(yscale)) yscale <- rg + c(-0.1, 0.1) * r

  ## panel function for profile plots in nodes
  panelfun <- function (node) {

    ## node index
    idn <- id_node(node)

    ## get cfs and labels
    cfi <- cf[[idn]]
    if(any(!is.finite(cfi))) {
      cfi[cfi < 0 & !is.finite(cfi)] <- yscale[1]
      cfi[cfi > 0 & !is.finite(cfi)] <- yscale[2]
    }
    nmsi <- nms[[idn]]

    if(is.list(border)) {
      if(!all(names(border) %in% names(cf))) {warning("List 'border' is not named after the nodes")}
      border <- border[[as.character(idn)]]
    }
    if(is.list(col)) {
      if(!all(names(col) %in% names(cf))) {warning("List 'col' is not named after the nodes")}
      col <- col[[as.character(idn)]]
    }
    if(is.list(cex)) {
      if(!all(names(cex) %in% names(cf))) {warning("List 'cex' is not named after the nodes")}
      cex <- cex[[as.character(idn)]]
    }
    if(is.list(pch)) {
      if(!all(names(pch) %in% names(cf))) {warning("List 'pch' is not named after the nodes")}
      pch <- pch[[as.character(idn)]]
    }
    if(is.list(bg)) {
      if(!all(names(bg) %in% names(cf))) {warning("List 'bg' is not named after the nodes")}
      bg <- bg[[as.character(idn)]]
    }

    ## viewport setup
    top_vp <- viewport(layout = grid.layout(nrow = 2, ncol = 3,
                                            widths = unit(c(ylines, 1, 1), c("lines", "null", "lines")),
                                            heights = unit(c(1, 1), c("lines", "null"))),
                       width = unit(1, "npc"), height = unit(1, "npc") - unit(2, "lines"),
                       name = paste("node_profileplot", idn, sep = ""))
    pushViewport(top_vp)
    grid.rect(gp = gpar(fill = "white", col = 0))

    ## main title
    top <- viewport(layout.pos.col = 2, layout.pos.row = 1)
    pushViewport(top)
    mainlab <- paste(ifelse(id, paste("Node", idn, "(n = "), ""),
                     info_node(node)$nobs, ifelse(idn, ")", ""), sep = "")
    grid.text(mainlab)
    popViewport()

    ## actual plot
    plot_vpi <- viewport(layout.pos.col = 2, layout.pos.row = 2, xscale = xscale, yscale = yscale,
                         name = paste("node_profileplot", idn, "plot", sep = ""))
    pushViewport(plot_vpi)
    ## remove the vertical grey line that is the mean of the item parameters and replace by line at 0
    # grid.lines(xscale, c(mean(cfi), mean(cfi)), gp = gpar(col = refcol), default.units = "native")
    if(index) {
      if (what == "thresholds") {
        for (j in 1:ncol(cfi)) {
          grid.lines(x, cfi[, j], gp = gpar(col = linecol, lty = 2), default.units = "native")
          grid.text(label = paste0("C", j), x, cfi[, j], gp = gpar(col = col), default.units = "native")
        }
      } else {
        grid.lines(x, cfi, gp = gpar(col = linecol, lty = 2), default.units = "native")
        grid.points(x, cfi, gp = gpar(col = border, fill = col, cex = cex), pch = pch, default.units = "native")
      }
      grid.xaxis(at = x, label = nmsi)
    } else {
      if (what == "thresholds") {
        for (j in 1:ncol(cfi)) grid.text(paste0(nmsi, "-C", j), x[j], y = cfi[, j], default.units = "native")
      } else {
        grid.text(nmsi, x = x, y = cfi, default.units = "native")
      }
    }
    grid.yaxis(at = c(ceiling(yscale[1] * 100)/100, floor(yscale[2] * 100)/100))
    grid.rect(gp = gpar(fill = bg, alpha = .3))

    upViewport(2)
  }

  return(panelfun)
}
class(node_profileplot) <- "grapcon_generator"
