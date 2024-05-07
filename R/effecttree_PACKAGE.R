#' effecttree: Helper functions to use effect size measures
#' as a stopping criterion in model-based recursive partitioning trees
#'
#' @name effecttree-package
#' @aliases effecttree
#' @author Mirka Henninger & Jan Radek
#'
#' @description Helper functions to use the Mantel-Haenszel effect size measure
#' and the partial gamma coefficient as a stopping criterion in Rasch trees and
#' partial credit trees
#'
#' @import psychotools
#' @import partykit
#' @import psychotree
#' @import grid
#' @import stats
#' @import Formula
#' @import mvtnorm
#' @import rpart
#' @import sandwich
#' @import strucchange
#' @import survival
#' @import vcd
#' @importFrom difR mantelHaenszel
#' @importFrom iarm partgam_DIF
#' @importFrom utils capture.output
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom grDevices gray.colors
#' @importFrom graphics boxplot
#' @importFrom graphics hist
#' @importFrom psychotree node_profileplot
#' @importFrom partykit node_inner
#' @importFrom partykit node_party
#' @importFrom partykit kids_node
#' @importFrom partykit nodeids
#' @importFrom partykit nodeapply
#' @importFrom partykit info_node
#' @importFrom partykit data_party
#' @importFrom partykit is.terminal
#' @importFrom partykit id_node
#' @importFrom partykit split_node
#' @importFrom SimDesign quiet
#' @importFrom grDevices hcl.colors
#' @importFrom grDevices rainbow
#'
#'
NULL
