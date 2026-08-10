# The kept-list: a derivation *input*, never baked in.
#
# PRD story 10 + issue #198: the kept-equipment selection is a downstream
# derivation on the matrix, reviewed against the BPE 2025 universe. The BPE 2025
# kept-list is NOT decided yet — it is #198's exercise, to be made against the
# acquired universe (ticket 05). The derivation functions therefore require
# `kept` as an explicit parameter; this file only documents the legacy flagship's
# BPE 2024 starting point for that review.

#' The legacy flagship's kept TYPEQU codes — BPE 2024 vintage, reference only.
#'
#' These are the 54 codes the flagship kept under the BPE 2024 nomenclature
#' (`equipements_retenus.csv`). They are the starting point for #198's review,
#' NOT the BPE 2025 answer. Pass the reviewed list explicitly to the derivation
#' functions once #198 lands.
kept_list_bpe2024 <- function() {
  f <- system.file("extdata", "kept-list-bpe2024.csv", package = "batimentsequipements")
  if (!nzchar(f)) {
    stop("kept-list-bpe2024.csv not found in inst/extdata", call. = FALSE)
  }
  as.character(data.table::fread(f)[["all_desc"]])
}
