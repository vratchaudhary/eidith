library(httr)
library(curl)

h <- new_handle()
handle_setopt(h, .list = c(username=Sys.getenv("EIDITH_USERNAME"), password=Sys.getenv("EIDITH_PASSWORD"), progress()$options))
con <- curl("https://predict2api.eidith.org/api/app/Event", handle=h)
rr = readLines(con, warn=FALSE)
an <- ed_animals()
an %>% filter(is.na(taxagroup) | taxagroup=="Unknown" |is.na(order) | is.na(class) | is.na(species_scientific_name)) %>% select(species_scientific_name, species_common_name_english, taxagroup, class, order, family, genus, species, subspecies, binomial) -> amm


ama %>% filter(is.na(taxagroup) | taxagroup=="Unknown") %>% select(species_scientific_name, species_common_name_english, taxagroup, class, order, family, genus, species, subspecies, binomial) %>%  print(n=100)
