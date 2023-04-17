################################################################################
### PREREQUISITE: run loadData.R first to generate the bookSummaries dataframe
################################################################################
#-------------------------------------------------------------------------------
# check for dependencies and load packages
#-------------------------------------------------------------------------------
# Package names
packages <- c("tidyverse","psych","Hmisc","koRpus")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

################################################################################
### HYPOTHESIS 1
################################################################################
# Hypothesis 1: Males have more book authorships than females within the Wikipedia database.

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

################################################################################
### HYPOTHESIS 2
################################################################################
# Hypothesis 2: Despite Wikipedia’s policy on unbiasedness (“Wikipedia:Neutral point of view,” 2023), 
# some degree of bias is evident when it comes to book subjects and themes.
#-------------------------------------------------------------------------------
# Analysis 1: length of plot summaries
#-------------------------------------------------------------------------------
# We will test this hypothesis by counting the number of words that editors have written in the book plot summaries.

# add a rough word count
bookSummaries$summaryWordCount <- str_count(bookSummaries$plotSummary, '\\w+')

# this statement is broken!!!!! TODO: FIX ME
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
library(Hmisc)

res2 <- rcorr(as.matrix(bookSummaries[,c("authorSexInt","summaryWordCount")]))

res2$r

res2$P 

# p = 0.1224107 so there is no statistical significance

#-------------------------------------------------------------------------------
# Analysis 2: readability level
#-------------------------------------------------------------------------------
install.koRpus.lang("en")

library(koRpus.lang.en)

################################################################################
### HYPOTHESIS 4
################################################################################
# Hypothesis 4: Authors with higher femaleness write more inclusively (characters including more gender diversity) than authors with higher maleness. 

# For each plotSummary, extract the potential names of any characters ( Capital letter + at least one lowercase letter )

# For each character, analyze nameMaleness/nameFemaleness as in Hypothesis 1 and assign a characterSex

# Calculate the number of male, female, and indeterminate characters mentioned in each plot summary

# Do correlation analysis: count of M characters, count of F characters, ratio of M/F characters



