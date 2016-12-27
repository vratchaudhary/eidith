


# Data dimensions ---------------------------------------------------------

#' Number of unique animalIDs
#'
#' @export

pr_num <- function(df) {
  d <- nrow(df)
  res <- sprintf("There are %s unique animalIDs in this dataset", d)
  print(res)
}



# Co-infections ------------------------------------------------------------

#' Coinfections in the data
#'
#' @export

# Gets animalIDs with coinfections, sorted by decreasig number of coinfections
pr_coinfections <- function(df) {
  coinf <- df[ df$Num_viruses > 1, c("animalID", "SampleDate", "SiteName", "Country", "Pos_tests", "Num_viruses", "Viruses")]
  coinf <- coinf[with(coinf, order(-Num_viruses)), ]
  coinf
}



# Suite of tests ----------------------------------------------------------

#' Table of suite of tests conducted
#'
#' @export

pr_testsuites <- function(df) {
  tb <- sort(table(df$testTypes))
  suite <- as.data.frame.table(tb)
  colnames(suite) <- c("testTypes", "Num")
  suite
}



# Sampling timeline -------------------------------------------------------

#' Sampling timeline - breaks the down to year and month, and the numbers of animals tested in
#' them.
#'
#' @export

pr_timeline <- function(df) {
  dfdate <- df$SampleDate
  isnull <- dfdate[dfdate == "NULL"]
  ifelse(length(isnull) > 0, print(sprintf("There are %s NULL entries. The timeline for the rest is:",
                                           length(isnull)), quote = FALSE),
         print("There are no NULL entries", quote = FALSE))
  dfdate <- dfdate[ dfdate != "NULL"]
  date <- data.frame(date = as.Date(dfdate, "%m/%d/%y"))
  if(sum(is.na(date)) == nrow(date)) date <- data.frame(date = as.Date(dfdate, "%Y-%m-%d"))
  date$month <- factor(format(date$date, "%b"), levels = month.abb)
  date$year <- format(date$date, "%Y")
  tb <- table(date$year, date$month)
  tb <- addmargins(tb)
  tb
}




# Positive tests ----------------------------------------------------------

#' Positive test type combinations
#'
#' @export

pr_posTests <- function(df) {
  tb <- sort(table(df$Pos_tests), decreasing = TRUE)
  posTests <- as.data.frame.table(tb, stringsAsFactors = FALSE)
  colnames(posTests) <- c("pos_testTypes", "Num")
  posTests
}




# Virus combinations -----------------------------------------------------------

#' Tabulation of the various viruses found, and the combinations in which they
#' occurred.
#'
#' @export

pr_combViruses <- function(df) {
  tb <- sort(table(df$Viruses), decreasing = TRUE)
  virComb <- as.data.frame.table(tb, stringsAsFactors = FALSE)
  colnames(virComb) <- c("Viruses", "Num")
  virComb
}




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




# Sites -------------------------------------------------------------------

#' Breakdown of which sites had animals that tested positive or negative.
#'
#' @export

pr_sites <- function(df) {
  tb <- table(df$SiteName, df$Positive)
  colnames(tb) <- c("Negative", "Positive")
  tb <- addmargins(tb, FUN = list(Total = sum), quiet = TRUE)
  tb
}




#  individual Test Types or Viruses from their tables ---------------------

# =============================================================================
# Function to get counts of individual Test Types or Viruses from their tables:
# =============================================================================

name_fun = function(name, num) {
  vSplit = strsplit(name, split = "; ")[[1]]
  vSplit = data.frame(Names = vSplit, stringsAsFactors = FALSE)
  vSplit$N = num
  vSplit
}


ind_name_fun = function(df) {
  Names = Map(name_fun, df[[1]], df[[2]])
  # viruses = do.call(rbind, viruses)
  Names
}


individual_fun = function(df) {

  all_names = ind_name_fun(df)
  names(all_names) = NULL
  all_names = do.call(rbind, all_names)

  agg = aggregate(N ~ Names, data = all_names, FUN = sum)
  agg = agg[ order(-agg$N), ]
  agg
}



# Positive test-types

#' Positive test-types
#' Tabular form of individual tests that were positive.
#'
#' @export

pr_indTests <- function(df) {
  df <- pr_posTests(df)
  ind <- individual_fun(df)
  ind
}


# Individual viruses

#'Viruses in data
#'Tabluar form of which viruses we found (and their numbers).
#'
#' @export

pr_indViruses <- function(df) {
  df <- pr_combViruses(df)
  ind <- individual_fun(df)
  ind
}




# Mapping -----------------------------------------------------------------

#' Map with sitename name, numbers sampled and positive
#'
#' @export

pr_mapSite <- function(df) {
  df$A = 1 # To get number of animals/animalIDs. A for "All"
  df$P = ifelse(df$Positive == "Yes", 1, 0) # To get num positives
  aggSite = aggregate(cbind(A, P) ~ SiteName + Latitude + Longitude, data = df, FUN = sum)
  aggSite$popup = sprintf("%s <br> n = %s <br> positives = %s", aggSite$SiteName, aggSite$A, aggSite$P)
  aggSite$color = ifelse(aggSite$P > 0, "red", "blue")
  map = leaflet::leaflet()
  map = leaflet::addTiles(map)
  map = leaflet::addCircleMarkers(map, data = aggSite, radius = 5,
                                  popup =~ paste0(popup),
                                  color = aggSite$color)
  map
}


#' Map to visualize aggregate sampling numbers
#' @export

pr_mapAgg <- function(df) {
  map = leaflet::leaflet()
  map = leaflet::addTiles(map)
  map = leaflet::addCircleMarkers(map, data = df,
                                  radius = 5,
                                  clusterOptions = leaflet::markerClusterOptions())
  map
}



#' Map with toggle option for site info and aggregate numbers
#' @export

pr_mapSiteAgg <- function(df) {
  df$A = 1 # To get number of animals/animalIDs. A for "All"
  df$P = ifelse(df$Positive == "Yes", 1, 0) # To get num positives
  aggSite = aggregate(cbind(A, P) ~ SiteName + Latitude + Longitude, data = df, FUN = sum)
  aggSite$color = ifelse(aggSite$P > 0, "red", "blue")

  aggSite$popup = sprintf("%s <br> n = %s <br> positives = %s", aggSite$SiteName, aggSite$A, aggSite$P)
  counter = 0
  overlayGroups = NA
  map = leaflet::leaflet()
  map = leaflet::addTiles(map)

  map = leaflet::addCircleMarkers(map, data = aggSite, radius = 5,
                                  popup =~ paste0(popup), group = "Sites",
                                  color = aggSite$color)
  counter = counter + 1
  overlayGroups[counter] = "Sites"

  map = leaflet::addCircleMarkers(map, data = df, clusterOptions = leaflet::markerClusterOptions(),
                                  group = "Aggregate Num")
  counter = counter + 1
  overlayGroups[counter] = "Aggregate Num"

  map = leaflet::addLayersControl(map, overlayGroups = overlayGroups,
                                  options = leaflet::layersControlOptions(collapsed = FALSE))
  map
}



# Save map ----------------------------------------------------------------


#' Save created maps
#' @export

save_map <- function(map, fname) {
  htmlwidgets::saveWidget(map, paste0(fname, ".html"))
}




######################################## Smaller functions



# Basic housekeeping ------------------------------------------------------

basic_housekeeping <- function(df) {
  # Basic housekeeping:
  # ==================
  # Checking and correcting col names with .. to _
  cNames = names(df)
  toChange = grepl("\\.|\\.\\.", cNames)
  cNames[toChange]
  gsub("\\.|\\.\\.", "_", cNames)[toChange]
  cNames[toChange] = gsub("\\.|\\.\\.", "_", cNames)[toChange]

  # changing column name "AnimalID_GAINS_" to "animalID" - for simplicity
  cNames[ cNames == "AnimalID_GAINS_" ] = "animalID"

  names(df) = cNames

  # Keeping only those with "polymerase chain reaction"
  df = subset(df, df$TestTypeBroad == "Polymerase_Chain_Reaction")
  df
}



# Getting taxagroups in data ----------------------------------------------

get_taxa  <- function(df) {
  taxagps = data.frame(taxa = unique(df$Taxagroup))

  # IMP: Check for taxa not listed below
  taxas = data.frame(taxa = c("Bats", "Birds", "Carnivores", "Non-human Primates",
                              "Other Mammals", "Reptiles", "Rodents & Shrews",
                              "Ungulates", "Unknown"),
                     abbrev = c("Bat", "Bird", "Carn", "NHP", "Other", "Rept",
                                "RnS", "Ung", "Unkn"),
                     stringsAsFactors = FALSE)

  taxagps = merge(taxagps, taxas, by = "taxa", all.x = TRUE)
  taxagps
}



# Subset and save by taxa (raw data) --------------------------------------

# Here, countryabbr is how I want country to be used in file name, e.g. "ug" for "Uganda"
tx_subFun <- function(n, taxa, abbr, data, countryabbr) {
  tx = taxa[n]
  subset = data[data$Taxagroup == tx, ]
  saveRDS(subset, sprintf("Data/%s_%s-raw-%s.RDS", Sys.Date(), countryabbr, abbr[n]))
  return("done!")
}



# Get animal info ---------------------------------------------------------

getAnimalInfo =
  function(animal, dataset, viruses)
  {
    df = dataset[dataset$animalID == animal, ]
    general = generalInfo(df, animal)
    specific = lapply(viruses, testChecker, df)
    specific = do.call(cbind, specific)
    animalInfo = cbind(general, specific)
    animalInfo
  }



# General Info function ------------------------------------------------------------

generalInfo =
  function(df, animal)
  {

    # Specimens collected:
    specimens = paste0(unique(df$SpecimenType), collapse = "; ") #***

    # Number of unique viruses/viral families tested:
    Num_testTypes = length(unique(df$TestRequested))

    # Names of unique viruses/viral families tested:
    testTypes = paste0(sort(unique(df$TestRequested)), collapse = "; ")

    # Did animal test positive for anything?
    if(sum(df$ConfirmationResult %in% "Positive") > 0) Positive = "Yes" else Positive = "No"

    # Which viruses (and how many) did we detect?
    if(Positive == "Yes") {
      Pos_tests = paste0(sort(unique(df$TestRequested [df$VirusName != "NULL"] )), collapse = "; ")
      Viruses = paste0(sort(unique(df$VirusName [df$VirusName != "NULL"] )), collapse = "; ")
      Num_viruses = length(unique(df$VirusName [df$VirusName != "NULL"] ))
      # Interpretation = paste0(sort(unique(df$Interpretation [df$VirusName != "NULL"] )), collapse = "; \n")
    } else {
      Pos_tests = NA_character_
      Viruses = NA_character_
      Num_viruses = 0
      Interpretation = NA_character_
    }
    general = data.frame(animalID = animal, specimens, Num_testTypes, testTypes, Positive, Pos_tests, Viruses, Num_viruses, stringsAsFactors = FALSE)
  }




# Test specific info function ------------------------------------------------------

testChecker <- function(virus, df) {
  # Was animal tested for specific virus/viral family?
  vdf = df[df$TestRequested %in% virus, ]
  tested = nrow(vdf)
  if(tested > 0) {
    Tested = "Yes"
    # Was animal positive for specific test?
    # Which viruses (within specific test) and how many did we detect?
    if(sum(vdf$ConfirmationResult %in% "Positive") > 0) {
      Positive = "Yes"
      Viruses = paste0(unique(vdf$VirusName [vdf$VirusName != "NULL" & vdf$TestRequested == virus] ), collapse = "; ")
      Viruses_Num = length(Viruses)
    } else {
      Positive = "No"
      Viruses = NA_character_
      Viruses_Num = 0
    }
  } else { # If animal was not tested for specific virus/viral family:
    Tested = "No"
    Positive = NA_character_
    Viruses = NA_character_
    Viruses_Num = NA_integer_
  }
  specific = data.frame(Tested, Positive, Viruses, Viruses_Num, stringsAsFactors = FALSE)
  virus = gsub(" ", "", virus)
  names(specific) = paste0(virus, "_", names(specific))
  return(specific)
}





