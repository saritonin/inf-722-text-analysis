install.packages("tidyverse")
library(tidyverse)

setwd("~/Documents/School/PhD/2023-01 Spring Semester/INF 722 - Information Organization/TextAnalysisProject")

bookSummaries <- read_tsv("booksummaries/booksummaries.txt", col_names = FALSE)

names(bookSummaries) <- names(bookSummaries) <- c("wikipediaArticleId","freebaseId","bookTitle","author","publicationDate","bookGenres","plotSummary")

# split out author first name
# author first name must be at least 2 letters followed by a space

bookSummaries$authorFirst <- trimws(str_extract(bookSummaries$author,"^[:alpha:][:alpha:]+[:space:]"))

bookSummaries %>% filter(!is.na(authorFirst)) %>% nrow() # 13303 records

bookSummaries %>% filter(!is.na(authorFirst)) %>%  count(authorFirst) %>% arrange(desc(n)) # John = 478

# load the genderByName dataset
genderByName <- read_csv("genderByName/name_gender_dataset.csv")

genderByName %>% count(Name) %>% arrange(desc(n))

probableGender <- as.data.frame(unique(genderByName$Name))

names(probableGender) <- "name"

# retrieve number of males with a given name
genderByName[genderByName$Gender == 'M',c(1,3)]

probableGender <- left_join(probableGender,genderByName[genderByName$Gender == 'M',c(1,3)], by=c("name" = "Name"))

names(probableGender) <- c("name","countM")

# retrieve number of females with a given name
probableGender <- left_join(probableGender,genderByName[genderByName$Gender == 'F',c(1,3)], by=c("name" = "Name"))

names(probableGender) <- c("name","countM","countF")

# calculate name "maleness" - likelihood the name is associated with a Male vs Female
probableGender$nameMaleness = (probableGender$countM)/(probableGender$countF + probableGender$countM)

probableGender %>% arrange(desc(nameMaleness))

# join nameMaleness into the bookSummaries data
bookSummaries <- left_join(bookSummaries, probableGender, by=c("authorFirst" = "name"), )

# remove extraneous columns
bookSummaries$countM <- NULL
bookSummaries$countF <- NULL

# verify join results
bookSummaries %>% filter(!is.na(nameMaleness)) %>% nrow() # 11098

bookSummaries  %>% filter(!is.na(nameMaleness)) %>% summarize(meanMaleness = mean(nameMaleness)) # 0.713

# add a rough word count
bookSummaries$summaryWordCount <- str_count(bookSummaries$plotSummary, '\\w+')

bookSummaries %>% filter(!is.na(nameMaleness)) %>% summarize(totalWords = sum(summaryWordCount))


