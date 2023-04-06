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

bookSummaries %>% filter(!is.na(authorFirst)) %>% nrow() # 13303 records

bookSummaries %>% filter(!is.na(authorFirst)) %>%  count(authorFirst) %>% arrange(desc(n)) # John = 478

# describe the sexByName dataset
sexByName %>% count(name) %>% arrange(desc(n))

# create a new data frame for data about the probable sex associated with a given name
probableSex <- as.data.frame(unique(sexByName$name))

names(probableSex) <- c("name")

# retrieve number of males with a given name
sexByName[sexByName$sex == 'M',c(1,3)]

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
probableSex %>% arrange(desc(nameMaleness))

# join nameMaleness into the bookSummaries data
bookSummaries <- left_join(bookSummaries, probableSex, by=c("authorFirst" = "name"), )

# remove extraneous columns
bookSummaries$countM <- NULL
bookSummaries$countF <- NULL

# verify join results
bookSummaries %>% filter(!is.na(nameMaleness)) %>% nrow() # 11098

bookSummaries %>% filter(!is.na(nameMaleness)) %>% summarize(meanMaleness = mean(nameMaleness)) # 0.713

# add a authorSex column to the bookSummaries dataframe
bookSummaries$authorSex <- case_when(bookSummaries$nameMaleness >= 0.60 ~ "Male",
                                     bookSummaries$nameFemaleness >= 0.60 ~ "Female",
                                     TRUE ~ "Indeterminate")

# plot maleness vs femaleness
ggplot(bookSummaries, aes(x = authorSex)) + 
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")+
  labs(title = "Corpus authorship by sex") +
  theme(
    panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg - remove color tag for border
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major.y = element_line(color = "grey60"), # leave y major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

# add a rough word count
bookSummaries$summaryWordCount <- str_count(bookSummaries$plotSummary, '\\w+')

wordCounts <- bookSummaries %>% group_by(authorSex) %>% summarize(meanWords = mean(summaryWordCount))

# plot summary word count by authorSex
ggplot(wordCounts, aes(x = authorSex, y = meanWords)) + 
  geom_col() +
  geom_text(aes(label = meanWords), vjust = 1.5, color = "white")+
  labs(title = "Average word count of plot summary by author's sex") +
  theme(
    panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg - remove color tag for border
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major.y = element_line(color = "grey60"), # leave y major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

# run a correlation for author sex vs summary word count
# first, need to make a dummy variable for authorSex

bookSummaries$authorSexInt <- NULL

bookSummaries$authorSexInt <- ifelse(bookSummaries$authorSex == 'Male',1,
                                     ifelse(bookSummaries$authorSex == 'Female',0,NA))

#-------------------------------------------------------------------------------
# different correlation testing options
#-------------------------------------------------------------------------------
library(psych)

corr.test(bookSummaries$authorSexInt, bookSummaries$summaryWordCount)
# > corr.test(bookSummaries$authorSexInt, bookSummaries$summaryWordCount)
# Call:corr.test(x = bookSummaries$authorSexInt, y = bookSummaries$summaryWordCount)
# Correlation matrix 
# [1] 0.01            mild positive correlation -- males have more words
# 
# Sample Size 
# [1] 12685
# 
# These are the unadjusted probability values.
# The probability values  adjusted for multiple tests are in the p.adj object. 
# [1] 0.12            result is not statistically significant

# stats package cor
cor(bookSummaries[,c("authorSexInt","summaryWordCount")], method = c("pearson"), use = "complete.obs")

# Hmisc for detailed rho and p values
install.packages("Hmisc")
library(Hmisc)

res2 <- rcorr(as.matrix(bookSummaries[,c("authorSexInt","summaryWordCount")]))

res2$r

res2$P 

# p = 0.1224107 so there is no statistical significance