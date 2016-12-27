+ed_tests_report <- function(status = c("Result finalized, interpretation pending"),
                             +                            test_ids = NULL,
                             +                            meta_fields = c(sp_idname="specimen_id_name",
                                                                          +                                            sp="species_scientific_name",
                                                                          +                                            country="country",
                                                                          +                                            an_idname="animal_id_name",
                                                                          +                                            lab="diag_lab_shortname",
                                                                          +                                            test_rq="test_requested",
                                                                          +                                            meth="methodology_reference",
                                                                          +                                            seq_id="virus_id",
                                                                          +                                            test_id="test_id")) {
  +  ## First we select tests for which interpretation isn't complete and aren't pool positives
    +  if(!is.null(test_ids)) {
      +    tests <- ed_table_("tests", .dots=c(~test_id %in% test_ids))
      +  } else if (length(status) == 1) {
        +    tests <- ed_table_("tests", .dots=c(~test_status == status))
        +  } else if (length(status) > 1) {
          +    tests <- ed_table_("tests", .dots=c(~test_status %in% status))
          +  } else {
            +    stop("Neither status nor test_ids specified.")
            +  }
  +  miss_seq <- tests[["test_id"]][is.na(tests[["sequence"]])]
  +  if(length(miss_seq)) {
    +    warning("Missing sequences in tests ", paste(miss_seq, collapse = ", "))
    +  }
  +  # Then join together with other data to get other fields for metadata
    +  test_spec <- ed_table_("test_specimen_ids", ~test_id %in% tests[["test_id"]])
    +  spec <- ed_table_("specimens", ~specimen_id %in% test_spec[["specimen_id"]])
    +  anim <- ed_table_("animals", ~animal_id %in% spec[["animal_id"]])
    +  events <- ed_table_("events", ~event_id %in% anim[["event_id"]])
    +  viruses <- ed_table_("viruses", ~test_id %in% tests[["test_id"]]) %>%
      +    select_(.dots=c("virus_id", "test_id"))
    +
      +  combined_tables <- left_join(tests, test_spec, by="test_id") %>%
        +    right_join(spec, by="specimen_id") %>%
        +    right_join(anim, by="animal_id") %>%
        +    right_join(events, by="event_id") %>%
        +    left_join(viruses, by="test_id")
      +
        +  combined_tables %>%
        +    group_by_("test_id") %>%
        +    ## These are the fields that end up in the metadata of the report
        +    summarise_at(.cols = c(meta_fields[!(meta_fields == "test_id")], "sequence"),
                          +                 .funs = funs_(dots='paste(unique(.), collapse=",")')) %>%
        +    group_by_() %>%
        +    select_(.dots=c(coalesce(na_if(names(meta_fields), ""), meta_fields), "sequence"))
