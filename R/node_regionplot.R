node_regionplot <- function (mobobj, names = FALSE, abbreviate = TRUE, type = c("mode",
                                                             "median", "mean"), ref = NULL, ylim = NULL, off = 0.1,
                             col_fun = gray.colors,
          bg = "white", uo_show = TRUE, uo_col = "red", uo_lty = 2,
          uo_lwd = 1.25, ylines = 2)
{
  stopifnot(!is.null(mobobj))
  stopifnot(off >= 0)
  type <- match.arg(type)
  threshparlst <- function(node) threshpar(node, ref = ref,
                                           type = type, alias = TRUE, relative = FALSE, cumulative = FALSE,
                                           vcov = FALSE)

  node <- nodeids(mobobj, terminal = TRUE)
  delta_lst <- partykit:::apply_to_models(mobobj, node, FUN = threshparlst,
                               ref = ref, type = type)
  m <- max(sapply(delta_lst, length))
  xi <- 0:m + c(0:(m - 1), m - 1) * off
  xlim <- c(xi[1], xi[m + 1])

  if(!is.list(col_fun)){
    col_fun_list <- rep(list(col_fun), m)
  } else{
    col_fun_list <- col_fun
  }
  if (is.null(ylim))
    ylim <- extendrange(unlist(delta_lst, use.names = FALSE),
                        f = 0.25)
  if (isTRUE(names)) {
    names <- lapply(delta_lst, names)
  }
  else if (is.character(names)) {
    names <- split(rep(names, length(node)), f = rep(1:length(node),
                                                     each = length(names)))
  }
  else {
    ncf <- lapply(delta_lst, NROW)
    names <- lapply(ncf, function(m) {
      lab <- rep("", m)
      lab[c(1, m)] <- c(1, m)
      pr <- pretty(1:m, n = 4)
      pr <- pr[pr > 1 & pr < m]
      lab[pr] <- pr
      lab
    })
    abbreviate <- FALSE
  }
  if (is.logical(abbreviate)) {
    nlab <- max(unlist(lapply(names, function(j) nchar(j))))
    abbreviate <- if (abbreviate)
      as.numeric(cut(nlab, c(-Inf, 1.5, 4.5, 7.5, Inf)))
    else nlab
  }
  names <- lapply(names, function(j) abbreviate(j, abbreviate))
  names(names) <- names(delta_lst) <- node
  panelfun <- function(node) {
    id <- as.character(id_node(node))
    delta_unsorted <- delta_lst[[id]]
    lab <- paste("node", id, sep = "")
    namesi <- names[[id]]
    delta_sorted <- delta_unsorted
    us <- sapply(delta_unsorted, is.unsorted)
    if (any(us)) {
      usj <- which(us)
      for (j in usj) {
        tpj <- delta_unsorted[[j]]
        nj <- length(tpj)
        for (i in 1:nj) {
          if (all(tpj[i] > tpj[(i + 1):nj])) {
            tpj[i] <- mean(tpj[i:nj])
            tpj <- tpj[-(i + 1:nj)]
            break
          }
        }
        while (is.unsorted(tpj)) {
          uo_pos <- which(diff(tpj) < 0)
          tpj[uo_pos] <- (tpj[uo_pos] + tpj[uo_pos +
                                              1])/2
          tpj <- tpj[-(uo_pos + 1)]
        }
        delta_sorted[[j]] <- tpj
      }
    }
    top.vp <- viewport(layout = grid.layout(nrow = 2, ncol = 1,
                                            widths = unit(1, "null"), heights = unit(c(1, 1),
                                                                                     c("lines", "null"))), width = unit(1, "npc"),
                       height = unit(1, "npc") - unit(2, "lines"), name = paste(lab,
                                                                                "_effects", sep = ""))
    pushViewport(top.vp)
    grid.rect(gp = gpar(fill = bg, col = 0), name = paste(lab,
                                                          "_border", sep = ""))
    pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1,
                          name = paste(lab, "_title_vp", sep = "")))
    grid.text(paste("Node ", id, " (n = ", info_node(node)$nobs,
                    ")", sep = ""), name = paste(lab, "_title", sep = ""))
    upViewport()
    pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2,
                          name = lab))
    lab <- paste(lab, "_plot", sep = "")
    wcol <- c(ylines, 1, 1)
    hrow <- c(0.5, 1, 1)
    top.vp <- viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                            widths = unit(wcol, c("lines", "null", "lines")),
                                            heights = unit(hrow, c("lines", "null", "lines"))),
                       name = paste(lab, "_top_vp", sep = ""))
    bmargin.vp <- viewport(layout.pos.row = 3, layout.pos.col = 2,
                           name = paste(lab, "_bottom-margin_vp", sep = ""))
    lmargin.vp <- viewport(layout.pos.row = 2, layout.pos.col = 1,
                           name = paste(lab, "_left-margin_vp", sep = ""))
    rmargin.vp <- viewport(layout.pos.row = 2, layout.pos.col = 3,
                           name = paste(lab, "_right-margin_vp", sep = ""))
    plot.vp <- viewport(layout.pos.row = 2, layout.pos.col = 2,
                        name = paste(lab, "_vp", sep = ""), xscale = xlim,
                        yscale = ylim)
    pushViewport(top.vp)
    pushViewport(plot.vp)
    for (j in seq_along(delta_sorted)) {
      ncat <- length(delta_sorted[[j]]) + 1
      grid.rect(x = rep.int(xi[j], ncat), y = c(ylim[1],
                                                delta_sorted[[j]]), width = rep.int(1, ncat),
                height = diff.default(c(ylim[1], delta_sorted[[j]],
                                        ylim[2])), just = c("left", "bottom"), gp = gpar(fill = col_fun_list[[j]](ncat)),
                default.units = "native", name = paste(lab, "_item",
                                                       j, "_rect", sep = ""))
    }
    if (uo_show && type == "mode") {
      uo_items <- which(!sapply(mapply(all.equal, delta_sorted,
                                       delta_unsorted, check.attributes = FALSE, SIMPLIFY = FALSE,
                                       USE.NAMES = FALSE), is.logical))
      for (j in uo_items) {
        uo_pars <- setdiff(delta_unsorted[[j]], delta_sorted[[j]])
        grid.polyline(x = rep(c(xi[j], xi[j] + 1), length(uo_pars)),
                      y = rep(uo_pars, each = 2), default.units = "native",
                      id = rep(1:length(uo_pars), each = 2), name = paste(lab,
                                                                          "item", j, "_uolines", sep = ""), gp = gpar(col = uo_col,
                                                                                                                      lwd = uo_lwd, lty = uo_lty))
      }
    }
    grid.rect(name = paste(lab, "_plot-box", sep = ""), gp = gpar(fill = NA))
    grid.xaxis(at = (xi[-(m + 1)] + 0.5), label = namesi,
               main = TRUE, name = paste(lab, "_xaxis-bottom", sep = ""))
    grid.yaxis(main = TRUE, name = paste(lab, "_yaxis-left",
                                         sep = ""))
    upViewport()
    pushViewport(lmargin.vp)
    upViewport(2)
    upViewport(2)
  }
  return(panelfun)
}
class(node_regionplot) <- "grapcon_generator"
