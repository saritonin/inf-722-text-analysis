################################################################################
### PREREQUISITE: run loadData.R first to generate the bookSummaries dataframe
################################################################################
#-------------------------------------------------------------------------------
# check for dependencies and load packages
#-------------------------------------------------------------------------------
# Package names
packages <- c("tidyverse","psych","Hmisc","koRpus","koRpus.lang.en")

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

# plot maleness vs femaleness
ggplot(bookSummaries %>% filter(!is.na(perceivedSex60)), aes(x = perceivedSex60, fill=perceivedSex60)) + 
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size=20)+
  # labs(title = "Corpus authorship by sex") + 
  theme(
    panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg - remove color tag for border
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major.y = element_line(color = "grey60"), # leave y major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

ggplot(bookSummaries %>% filter(!is.na(perceivedSex60)), aes(x = perceivedSex99, fill=perceivedSex99)) + 
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size=20)+
  # labs(title = "Corpus authorship by sex") + 
  theme(
    panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg - remove color tag for border
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major.y = element_line(color = "grey60"), # leave y major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

# plot maleness vs femaleness with various thresholds

# set some variables to make later line-drawing easier
thresholdTotalRows <- sum(thresholdEffect[thresholdEffect$thresholdValue=='60%',c('rowCount')])

thresholdM60 <- 
  thresholdEffect %>% 
  filter(thresholdValue=='60%' & perceivedSex=='Male') %>%
  pull(rowCount)

thresholdI60 <-
  thresholdEffect %>% 
  filter(thresholdValue=='60%' & perceivedSex=='Indeterminate') %>%
  pull(rowCount)

# normal line graph of the values
ggplot(thresholdEffect, aes(x=thresholdValue, y=rowCount, color=perceivedSex, fill=perceivedSex)) + 
  geom_line(aes(group=perceivedSex))+
  geom_point(size=2, shape=21)

# stacked area graph
ggplot(thresholdEffect, aes(x=thresholdValue, y=rowCount, color=perceivedSex, fill=perceivedSex)) + 
  geom_area(aes(group=perceivedSex),
            color="black",
            size=.2) +
  geom_hline(yintercept=thresholdTotalRows/2,linetype="dashed") +
  annotate("text",x=3.5,y=(thresholdTotalRows/2)+(thresholdTotalRows/30), label="50%")+
  geom_hline(yintercept=thresholdM60+(thresholdI60/2), linetype="dotted") +
  #labs(title="Effect of different threshold values on perceived sex of authors in the corpus")+
  theme_minimal()

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

wordCounts <- bookSummaries %>% group_by(perceivedSex60) %>% dplyr::summarize(meanWords = round(mean(summaryWordCount),digits=1))

# plot summary word count by authorSex
ggplot(wordCounts, aes(x = perceivedSex60, y = meanWords, fill=perceivedSex60)) + 
  geom_col() +
  geom_text(aes(label = meanWords), vjust = 1.5, color = "black", size=20)+
  # labs(title = "Average word count of plot summary by author's sex") +
  theme(
    panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg - remove color tag for border
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major.y = element_line(color = "grey60"), # leave y major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

wordCounts <- bookSummaries %>% group_by(perceivedSex99) %>% dplyr::summarize(meanWords = mean(summaryWordCount))

# plot summary word count by authorSex at 99% threshold
ggplot(wordCounts, aes(x = perceivedSex99, y = meanWords)) + 
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

bookSummaries$authorSexInt <- ifelse(bookSummaries$perceivedSex60 == 'Male',1,
                                     ifelse(bookSummaries$perceivedSex60 == 'Female',0,NA))

bookSummaries$authorSexInt99 <- NULL

bookSummaries$authorSexInt99 <- ifelse(bookSummaries$perceivedSex99 == 'Male',1,
                                     ifelse(bookSummaries$perceivedSex99 == 'Female',0,NA))

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

rcorr(as.matrix(bookSummaries[,c("authorSexInt","summaryWordCount")]))$r
rcorr(as.matrix(bookSummaries[,c("authorSexInt","summaryWordCount")]))$P

# p = 0.1224107 so there is no statistical significance

# recheck at 99%
corr.test(bookSummaries$authorSexInt99, bookSummaries$summaryWordCount)
cor(bookSummaries[,c("authorSexInt99","summaryWordCount")], method = c("pearson"), use = "complete.obs")
rcorr(as.matrix(bookSummaries[,c("authorSexInt99","summaryWordCount")]))$r
rcorr(as.matrix(bookSummaries[,c("authorSexInt99","summaryWordCount")]))$P

# recheck using nameMaleness
corr.test(bookSummaries$nameMaleness, bookSummaries$summaryWordCount)
cor(bookSummaries[,c("nameMaleness","summaryWordCount")], method = c("pearson"), use = "complete.obs")
rcorr(as.matrix(bookSummaries[,c("nameMaleness","summaryWordCount")]))$r
rcorr(as.matrix(bookSummaries[,c("nameMaleness","summaryWordCount")]))$P

#-------------------------------------------------------------------------------
# H2 CHECK MEDIAN AND OTHER STATS
#-------------------------------------------------------------------------------
wordCountStats <- bookSummaries %>% group_by(perceivedSex60) %>% 
  dplyr::summarize(meanWords = mean(summaryWordCount),
                   medianWords = round(median(summaryWordCount),digits=1),
                   maxWords = max(summaryWordCount),
                   minWords = min(summaryWordCount))

# plot summary word count by authorSex
ggplot(wordCountStats, aes(x = perceivedSex60, y = medianWords, fill=perceivedSex60)) + 
  geom_col() +
  geom_text(aes(label = medianWords), vjust = 1.5, color = "black", size=20)+
  # labs(title = "Median word count of plot summary by author's sex") +
  theme(
    panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg - remove color tag for border
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major.y = element_line(color = "grey60"), # leave y major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )


#-------------------------------------------------------------------------------
# H2 PLAY WITH SOME HISTOGRAMS
#-------------------------------------------------------------------------------
# Basic histogram
ggplot(bookSummaries, aes(x=summaryWordCount)) + geom_histogram() +
  geom_vline(aes(xintercept=mean(summaryWordCount)),
             color="blue", linetype="dashed", size=1)

# Histogram styled
ggplot(bookSummaries, aes(x=summaryWordCount)) + 
  geom_histogram(colour="black", fill="white", bins=50) +
  theme_minimal() +
  labs(title="Number of words in plot summary", x="Number of words")

# Histogram with density plot
ggplot(bookSummaries, aes(x=summaryWordCount)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

# Interleaved histograms with mean lines
ggplot(bookSummaries[bookSummaries$perceivedSex60=='Male'|bookSummaries$perceivedSex60=='Female',], aes(x=summaryWordCount, color=perceivedSex60)) +
  geom_histogram(fill="white", position="dodge")+
  geom_vline(data=wordCountStats, aes(xintercept=meanWords, color=perceivedSex60),
             linetype="dashed")+
  theme(legend.position="top")

#-------------------------------------------------------------------------------
# Analysis 2: readability level
#-------------------------------------------------------------------------------
# install.koRpus.lang("en")

# library(koRpus.lang.en)

################################################################################
### HYPOTHESIS 4
################################################################################
# Hypothesis 4: Authors with higher femaleness write more inclusively (characters including more gender diversity) than authors with higher maleness. 

# For each plotSummary, extract the potential names of any characters ( Capital letter + at least one lowercase letter )
bookSummaries$plotCharacters <- str_extract_all(bookSummaries$plotSummary,"\\b[:upper:][:lower:]+\\b") %>% sapply(unique)

# For each character, analyze nameMaleness/nameFemaleness as in Hypothesis 1 and assign a characterSex
# TODO: make this processing more efficient and/or make the "loop" through the dataframe better

getProbableSex <- probableSex$probableSex
names(getProbableSex) <- probableSex$name

fGetProbableSex <- function(nameToLookup) {
  returnedSex <- unname(getProbableSex[nameToLookup])
  return(returnedSex)
}

# Process the dataframe in pieces to ensure that we can save/resume as needed
bookSummaries$characterSexes[1:2000] <- 
  bookSummaries$plotCharacters[1:2000] %>% sapply(fGetProbableSex)

bookSummaries$characterSexes[2000:4000] <- 
  bookSummaries$plotCharacters[2000:4000] %>% sapply(fGetProbableSex)

bookSummaries$characterSexes[4000:6000] <- 
  bookSummaries$plotCharacters[4000:6000] %>% sapply(fGetProbableSex)

bookSummaries$characterSexes[6000:8000] <- 
  bookSummaries$plotCharacters[6000:8000] %>% sapply(fGetProbableSex)

bookSummaries$characterSexes[8000:10000] <- 
  bookSummaries$plotCharacters[8000:10000] %>% sapply(fGetProbableSex)

bookSummaries$characterSexes[10000:12000] <- 
  bookSummaries$plotCharacters[10000:12000] %>% sapply(fGetProbableSex)

bookSummaries$characterSexes[12000:14000] <- 
  bookSummaries$plotCharacters[12000:14000] %>% sapply(fGetProbableSex)

bookSummaries$characterSexes[14000:16559] <- 
  bookSummaries$plotCharacters[14000:16559] %>% sapply(fGetProbableSex)

# Calculate the number of male, female, and indeterminate characters mentioned in each plot summary
bookSummaries$charactersM <- bookSummaries$characterSexes %>% sapply(paste,"collapse='|'") %>% sapply(str_count,'Male') %>% sapply(sum)
  
bookSummaries$charactersF <- bookSummaries$characterSexes %>% sapply(paste,"collapse='|'") %>% sapply(str_count,'Female') %>% sapply(sum)
  
bookSummaries$charactersI <- bookSummaries$characterSexes %>% sapply(paste,"collapse='|'") %>% sapply(str_count,'Indeterminate') %>% sapply(sum)

# Do correlation analysis: count of M characters, count of F characters, ratio of M/F characters
bookSummaries$characterPctM <- bookSummaries$charactersM/(bookSummaries$charactersM + bookSummaries$charactersF)
bookSummaries$charactersMoreMInt <- case_when(bookSummaries$characterPctM > 0.5 ~ 1,
                                              TRUE ~ 0)
bookSummaries$charactersMuchMoreMInt <- case_when(bookSummaries$characterPctM >= 0.75 ~ 1,
                                                  TRUE ~ 0)

bookSummaries$characterSexImbalanceInt <- case_when(bookSummaries$characterPctM >= 0.75 ~ 1,
                                                    bookSummaries$characterPctM <= 0.25 ~ 1,
                                                    TRUE ~ 0)

bookSummaries$characterSexImbalanceText <- case_when(bookSummaries$characterPctM >= 0.90 ~ 'Characters are nearly all male',
                                                 bookSummaries$characterPctM >= 0.75 ~ 'Characters are mostly male',
                                                 bookSummaries$characterPctM > 0.25 ~ 'Character sexes are somewhat balanced',
                                                 bookSummaries$characterPctM > 0.10 ~ 'Characters are mostly female',
                                                 bookSummaries$characterPctM <= 0.10 ~ 'Characters are nearly all female',
                                                 TRUE ~ 'Unknown')

# more than 50% of characters are male
corr.test(bookSummaries$authorSexInt, bookSummaries$charactersMoreMInt)

chisq.test(bookSummaries$authorSexInt, bookSummaries$charactersMoreMInt)

# more than 75% of characters are male
corr.test(bookSummaries$authorSexInt, bookSummaries$charactersMuchMoreMInt)

chisq.test(bookSummaries$authorSexInt, bookSummaries$charactersMuchMoreMInt)

# more that 75% of characters are one gender
corr.test(bookSummaries$authorSexInt, bookSummaries$characterSexImbalanceInt)

chisq.test(bookSummaries$authorSexInt, bookSummaries$characterSexImbalanceInt)

chisq.test(bookSummaries$authorSexInt, bookSummaries$characterSexImbalanceInt)$observed


# attempt with more categories

hyp4dataSummary <- bookSummaries %>% filter(perceivedSex60 %in% c('Male','Female') & characterSexImbalanceText != 'Unknown') %>% group_by(perceivedSex60) %>% count(characterSexImbalanceText) %>% print(n=100)

hyp4data <- bookSummaries %>% filter(perceivedSex60 %in% c('Male','Female') & characterSexImbalanceText != 'Unknown') %>% select(perceivedSex60,characterSexImbalanceText)

hyp4chisq <- chisq.test(hyp4data$perceivedSex60,hyp4data$characterSexImbalanceText)

# ensure significance
hyp4chisq$p.value
# [1] 1.43658e-72

# check Pearson residuals
round(hyp4chisq$residuals,3)

# make a nice graph
library(corrplot)

corrplot(hyp4chisq$residuals,is.cor=FALSE)

# reorder the columns
hyp4data$characterSexImbalanceText <- as.factor(hyp4data$characterSexImbalanceText)
hyp4data$perceivedSex60 <- as.factor(hyp4data$perceivedSex60)

levels(hyp4data$characterSexImbalanceText)
hyp4data$characterSexImbalanceText <- factor(hyp4data$characterSexImbalanceText, levels=c('Characters are nearly all female',
                                                                                          'Characters are mostly female',
                                                                                          'Character sexes are somewhat balanced',
                                                                                          'Characters are mostly male',
                                                                                          'Characters are nearly all male'))

hyp4chisq <- chisq.test(hyp4data$perceivedSex60,hyp4data$characterSexImbalanceText)

corrplot(hyp4chisq$residuals,is.cor=FALSE)

?corrplot

