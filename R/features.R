# Species -----------------------------------------------------------------

#' Breakdown of which sampled species were positive or negative.
#'
#' @export

pr_species <- function(df) {
  tb <- table(df$SpeciesScientificName, df$Positive)
  colnames(tb) <- c("Negative", "Positive")
  tb <- addmargins(tb, FUN = list(Total = sum), quiet = TRUE)
  tb
}
