
rm(list=ls())

### LOAD PACKAGES
pacman::p_load("dplyr", "readxl", "openxlsx", "stringr", "tidyr", "pbapply","ggplot2", "readr")


###Load data
pubdata_raw <- read.csv("data/scopus_clean.csv")

# Function to change grouping var:
recat_doc_type <- function(x) {
  t <- tolower(trimws(as.character(x)))
  
  out <- case_when(
    t %in% c(
      "article",
      "article in press",
      "conference paper",
      "data paper",
      "letter",
      "note",
      "short survey"
    ) ~ "Article",
    
    t %in% c("book", "book chapter") ~ "Book",
    
    t == "review"    ~ "Review",
    t == "retracted" ~ "Retracted",
    
    TRUE ~ "Other"
  )
  
  factor(out, levels = c("Article", "Book", "Review", "Retracted", "Other"))
}
# Create clean group variable
pubdata <- pubdata_raw %>%  
  mutate(document_type = recat_doc_type(Document.Type))


#Define group variable for analysis
group_var = "document_type"
group_var_sym <- sym(group_var)


#### JOURNAL SUBJECT AREAS #### needed for scientific quality
{
sourcelist <- readRDS("data/sourcelist2020")

subjAreas <- pubdata_raw %>% 
  select(EID, issn = ISSN) %>% 
  left_join(select(sourcelist, issn, sjr, subjectArea), by = "issn") %>% distinct() %>% 
  group_by(subjectArea) %>% summarise(publications = n_distinct(EID)) %>% ungroup() %>% 
  arrange(desc(publications)) %>% na.omit
}

#### NUMBER OF OF PUBLICATIONS PER GROUP VARIABLE CATEGORY ####
{
# Overall  
pub_per_group_var <- pubdata %>%
  group_by(!!group_var_sym) %>%
  summarise(publications = n_distinct(EID)) %>%
  ungroup()


#Per year
pub_per_group_var_year <- pubdata %>%
  group_by(!!group_var_sym, Year) %>%
  summarise(publications = n_distinct(EID)) %>%
  ungroup()

}

#### SCIENTIFIC QUALITY ####
{
top_subjectareas <- subjAreas %>% 
  mutate(cum_share = cumsum(publications)/sum(publications)) %>% 
  filter(cum_share < 0.8) %>% 
  summarise(n = n() + 1) %>% pull(n)

top_80subjectareas <- subjAreas %>% 
  filter(1:n() <= top_subjectareas) %>% pull(subjectArea)

#Create benchmark percentiles
sjr_benchmark <- sourcelist %>% 
  filter(subjectArea %in% top_80subjectareas) %>% 
  select(issn, sjr, subjectArea) %>% distinct() %>% na.omit %>% 
  summarise(p99 = quantile(sjr, probs = 0.99),
            p95 = quantile(sjr, probs = 0.95),
            p90 = quantile(sjr, probs = 0.90))

scientificQuality <- pubdata %>% 
  select(EID, issn = ISSN) %>% 
  left_join(select(sourcelist, issn, sjr), by = "issn") %>% distinct() %>% 
  cbind(sjr_benchmark) %>% 
  mutate(top1 = ifelse(sjr >= p99, 1, 0),
         top5 = ifelse(sjr >= p95, 1, 0),
         top10 = ifelse(sjr >= p90, 1, 0)) %>% 
  summarise(top1 = sum(top1, na.rm = TRUE),
            top5 = sum(top5, na.rm = TRUE),
            top10 = sum(top10, na.rm = TRUE)) %>% 
  mutate(top1_share = top1/n_distinct(pubdata_raw$EID),
         top5_share = top5/n_distinct(pubdata_raw$EID),
         top10_share = top10/n_distinct(pubdata_raw$EID),
         publications = n_distinct(pubdata_raw$EID))


#### Group the data by group variable ####
quality_by_group_var <- pubdata %>% 
  select(EID, issn = ISSN, !!group_var_sym) %>% 
  left_join(select(sourcelist, issn, sjr), by = "issn") %>% distinct() %>% 
  cbind(sjr_benchmark) %>% 
  group_by(!!group_var_sym) %>%
  mutate(top1 = ifelse(sjr >= p99, 1, 0),
         top5 = ifelse(sjr >= p95, 1, 0),
         top10 = ifelse(sjr >= p90, 1, 0)) %>% 
  summarise(top1 = sum(top1, na.rm = TRUE),
            top5 = sum(top5, na.rm = TRUE),
            top10 = sum(top10, na.rm = TRUE),
            publications = n_distinct(EID)) %>%  # Calculate publications per subject
  mutate(top1_share = top1 / publications,
         top5_share = top5 / publications,
         top10_share = top10 / publications) %>%
  ungroup()
}


#### SCIENTIFIC IMPACT ####
# Add this code when we have benchmark


#### SAVE RESULTS ####
{

wb_results <- createWorkbook("wb_results")

writeData(wb_results, addWorksheet(wb_results, "Num pub by group var"), pub_per_group_var)
writeData(wb_results, addWorksheet(wb_results, "Num pub by group var year"), pub_per_group_var_year)
writeData(wb_results, addWorksheet(wb_results, "Quality by group var"), quality_by_group_var)



date_str <- format(Sys.Date(), "%Y%m%d")
saveWorkbook(wb_results, paste0("output/results_group_var_analysis_", date_str, ".xlsx"), overwrite = FALSE)


}

