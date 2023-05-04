#-------------------------------------------------------------------------------
# check for dependencies and load packages
#-------------------------------------------------------------------------------
# Package names
packages <- c("tidyverse")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

#-------------------------------------------------------------------------------
# Load data from text files
#-------------------------------------------------------------------------------
# Read data from file and set column names

# CMU Book Summary Dataset
bookSummaries <- read_tsv("data-sources/book-summaries/booksummaries/booksummaries.txt", col_names = FALSE)
names(bookSummaries) <- c("wikipediaArticleId","freebaseId","bookTitle","author","publicationDate","bookGenres","plotSummary")

# UCI Gender by Name Dataset
# Note: "gender" in the dataset refers to M/F assigned at birth.  
# This analysis uses "sex" to describe this information to align with current usage.
sexByName <- read_csv("data-sources/sex-by-name/name_gender_dataset.csv")
names(sexByName) <- c("name","sex","count","probability")

#-------------------------------------------------------------------------------
# Data preparation and merging
#-------------------------------------------------------------------------------
# split out author first name
# author first name must be at least 2 letters followed by a space

bookSummaries$authorFirst <- trimws(str_extract(bookSummaries$author,"^[:alpha:][:alpha:]+[:space:]"))

# bookSummaries %>% filter(!is.na(authorFirst)) %>% nrow() # 13303 records

# bookSummaries %>% filter(!is.na(authorFirst)) %>%  count(authorFirst) %>% arrange(desc(n)) # John = 478

# describe the sexByName dataset
# sexByName %>% count(name) %>% arrange(desc(n))

# create a new data frame for data about the probable sex associated with a given name
probableSex <- as.data.frame(unique(sexByName$name))

names(probableSex) <- c("name")

# retrieve number of males with a given name
# sexByName[sexByName$sex == 'M',c(1,3)]

probableSex <- left_join(probableSex,sexByName[sexByName$sex == 'M',c(1,3)], by=c("name"))

names(probableSex) <- c("name","countM")

# retrieve number of females with a given name
probableSex <- left_join(probableSex,sexByName[sexByName$sex == 'F',c(1,3)], by=c("name"))

names(probableSex) <- c("name","countM","countF")

# update counts for names associated with only M or only F
probableSex$countM[is.na(probableSex$countM)] <- 0
probableSex$countF[is.na(probableSex$countF)] <- 0

# calculate name "maleness" - likelihood the name is associated with a Male vs Female
probableSex$nameMaleness <- (probableSex$countM)/(probableSex$countF + probableSex$countM)

# calculate name "maleness" - likelihood the name is associated with a Female vs Male
probableSex$nameFemaleness <- (probableSex$countF)/(probableSex$countF + probableSex$countM)

# verify nameMaleness data
# probableSex %>% arrange(desc(nameMaleness))

# add a probableSex column with the values "Male" "Female" or "Indeterminate"
# leave this column in the dataset even though we are going more granular below
# this will ensure that older code still works while we are changing things
probableSex$probableSex <- case_when(probableSex$nameMaleness >= .6 ~ "Male",
                                     probableSex$nameFemaleness >= .6 ~ "Female",
                                     TRUE ~ "Indeterminate")

# add perceivedSex columns to check the effect of different thresholds
probableSex$perceivedSex60 <- case_when(probableSex$nameMaleness >= .6 ~ "Male",
                                        probableSex$nameFemaleness >= .6 ~ "Female",
                                        TRUE ~ "Indeterminate")

probableSex$perceivedSex70 <- case_when(probableSex$nameMaleness >= .7 ~ "Male",
                                        probableSex$nameFemaleness >= .7 ~ "Female",
                                        TRUE ~ "Indeterminate")

probableSex$perceivedSex80 <- case_when(probableSex$nameMaleness >= .8 ~ "Male",
                                        probableSex$nameFemaleness >= .8 ~ "Female",
                                        TRUE ~ "Indeterminate")

probableSex$perceivedSex90 <- case_when(probableSex$nameMaleness >= .9 ~ "Male",
                                        probableSex$nameFemaleness >= .9 ~ "Female",
                                        TRUE ~ "Indeterminate")

probableSex$perceivedSex95 <- case_when(probableSex$nameMaleness >= .95 ~ "Male",
                                        probableSex$nameFemaleness >= .95 ~ "Female",
                                        TRUE ~ "Indeterminate")

probableSex$perceivedSex99 <- case_when(probableSex$nameMaleness >= .99 ~ "Male",
                                        probableSex$nameFemaleness >= .99 ~ "Female",
                                        TRUE ~ "Indeterminate")

# verify descriptive label
# probableSex %>% count(probableSex)

# join probableSex into the bookSummaries data
bookSummaries <- left_join(bookSummaries, probableSex[,c('name','probableSex','perceivedSex60','perceivedSex70','perceivedSex80','perceivedSex90','perceivedSex95','perceivedSex99')], by=c("authorFirst" = "name"), )

# rename the "probableSex" column to be more clear for further processing
bookSummaries <- bookSummaries %>% rename(authorProbableSex = probableSex)

# check effect of perceivedSex higher threshold
# bookSummaries %>% 
#  filter(perceivedSex70 == 'Indeterminate' & authorProbableSex != 'Indeterminate') %>% count(authorProbableSex)
# RESULT of increasing threshold from 60% to 80%:
# 226 rows affected - 102 female and 124 male --> indeterminate

# RESULT of increasing threshold from 60% to 90%:
# 595 rows affected - 235 female and 360 male --> indeterminate

thresholdEffect <-
left_join(
  left_join(
    left_join(
      left_join(
        left_join(
          bookSummaries %>% count(perceivedSex60) %>% rename(perceivedSex = perceivedSex60, threshold60 = n),
          bookSummaries %>% count(perceivedSex70) %>% rename(perceivedSex = perceivedSex70, threshold70 = n)
          ),
        bookSummaries %>% count(perceivedSex80) %>% rename(perceivedSex = perceivedSex80, threshold80 = n)
      ),
      bookSummaries %>% count(perceivedSex90) %>% rename(perceivedSex = perceivedSex90, threshold90 = n)
    ),
    bookSummaries %>% count(perceivedSex95) %>% rename(perceivedSex = perceivedSex95, threshold95 = n)
  ),
  bookSummaries %>% count(perceivedSex99) %>% rename(perceivedSex = perceivedSex99, threshold99 = n)
)

thresholdEffect <- thresholdEffect %>% filter (!is.na(perceivedSex))

thresholdEffect <- pivot_longer(thresholdEffect, cols=2:7, names_to="thresholdValue", values_to ="rowCount")

thresholdEffect$thresholdValue <- case_when(thresholdEffect$thresholdValue == 'threshold60' ~ '60%',
                                            thresholdEffect$thresholdValue == 'threshold70' ~ '70%',
                                            thresholdEffect$thresholdValue == 'threshold80' ~ '80%',
                                            thresholdEffect$thresholdValue == 'threshold90' ~ '90%',
                                            thresholdEffect$thresholdValue == 'threshold95' ~ '95%',
                                            thresholdEffect$thresholdValue == 'threshold99' ~ '99%')

thresholdTotalRows <- sum(thresholdEffect[thresholdEffect$thresholdValue=='60%',c('rowCount')])

thresholdM60 <- 
  thresholdEffect %>% 
  filter(thresholdValue=='60%' & perceivedSex=='Male') %>%
  pull(rowCount)

thresholdI60 <-
  thresholdEffect %>% 
  filter(thresholdValue=='60%' & perceivedSex=='Indeterminate') %>%
  pull(rowCount)

ggplot(thresholdEffect, aes(x=thresholdValue, y=rowCount, color=perceivedSex, fill=perceivedSex)) + 
  geom_line(aes(group=perceivedSex))+
  geom_point(size=2, shape=21)

ggplot(thresholdEffect, aes(x=thresholdValue, y=rowCount, color=perceivedSex, fill=perceivedSex)) + 
  geom_area(aes(group=perceivedSex),
            color="black",
            size=.2) +
  geom_hline(yintercept=thresholdTotalRows/2,linetype="dashed") +
  annotate("text",x=3.5,y=(thresholdTotalRows/2)+(thresholdTotalRows/30), label="50%")+
  geom_hline(yintercept=thresholdM60+(thresholdI60/2), linetype="dotted") +
  #labs(title="Effect of different threshold values on perceived sex of authors in the corpus")+
  theme_minimal()
