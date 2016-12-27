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

lookup_animal <- function(animal_name){
  animal <- ed_table_("animals", ~animal_id_name == animal_name)
  event <- ed_table_("events", ~event_id == animal[["event_id"]])
  combined <- left_join(animal, event, by = "event_id")
  specimen <- ed_table_("specimens", ~animal_id == animal[["animal_id"]])
  if(dim(specimen)[1] == 0){
    return(combined)
  }else if(dim(specimen)[1] == 1) {
    testspecimen <- ed_table_("test_specimen_ids", ~specimen_id == specimen[["specimen_id"]])
  } else {
    testspecimen <- ed_table_("test_specimen_ids", ~specimen_id %in% specimen[["specimen_id"]])
  }
  combined <- right_join(combined, specimen, by = "animal_id")
  if(dim(testspecimen)[1] == 0){
    return(combined)
  }else if(dim(testspecimen)[1] == 1) {
    test <- ed_table_("tests", ~test_id == testspecimen[["test_id"]])
  } else {
    test <- ed_table_("tests", ~test_id %in% testspecimen[["test_id"]])
  }
  combined <- right_join(combined, testspecimen, by = "specimen_id")
  if(dim(test)[1] == 0){
    return(combined)
  } else if(dim(test)[1] == 1) {
    virus <- ed_table_("viruses", ~test_id == test[["test_id"]])
  }else {
    virus <- ed_table_("viruses", ~test_id %in% combined[["test_id"]])
  }
  combined <- right_join(combined, test, by = "test_id")
  if(dim(virus)[1] == 0){
    return(combined)
  } else {
  combined <- right_join(combined, virus, by = "test_id")
  }
  return(combined)
}


lookup_species <- function(binomial_species){
  animal <- ed_table_("animals", ~binomial == binomial_species)
  if(dim(animal)[1] == 0){
    stop("Species not found")
  } else if(dim(animal)[1] == 1) {
    event <- ed_table_("events", ~event_id == animal[["event_id"]])
  }else {
    event <- ed_table_("events", ~event_id %in% animal[["event_id"]])
  }
  combined <- left_join(animal, event, by = "event_id")
  if(dim(animal)[1] == 1) {
    specimen <- ed_table_("specimens", ~animal_id == animal[["animal_id"]])
  }else {
    specimen <- ed_table_("specimens", ~animal_id %in% animal[["animal_id"]])
  }
  if(dim(specimen)[1] == 0){
    return(combined)
  }else if(dim(specimen)[1] == 1) {
    testspecimen <- ed_table_("test_specimen_ids", ~specimen_id == specimen[["specimen_id"]])
  } else {
    testspecimen <- ed_table_("test_specimen_ids", ~specimen_id %in% specimen[["specimen_id"]])
  }
  combined <- right_join(combined, specimen, by = "animal_id")
  if(dim(testspecimen)[1] == 0){
    return(combined)
  }else if(dim(testspecimen)[1] == 1) {
    test <- ed_table_("tests", ~test_id == testspecimen[["test_id"]])
  } else {
    test <- ed_table_("tests", ~test_id %in% testspecimen[["test_id"]])
  }
  combined <- right_join(combined, testspecimen, by = "specimen_id")
  if(dim(test)[1] == 0){
    return(combined)
  } else if(dim(test)[1] == 1) {
    virus <- ed_table_("viruses", ~test_id == test[["test_id"]])
  }else {
    virus <- ed_table_("viruses", ~test_id %in% combined[["test_id"]])
  }
  combined <- right_join(combined, test, by = "test_id")
  if(dim(virus)[1] == 0){
    return(combined)
  } else {
    combined <- right_join(combined, virus, by = "test_id")
  }
  return(combined)
}

