library(dplyr)
library(stringr)
kneserNeySmoothingAlgorithmFinal <- function(inputString, oneWordWithStopWords, twoWordWithStopWords, threeWordWithStopWords)
{
  inputString <- tolower(inputString)
  inputString <- gsub("'d", " would", inputString)
  splitText <- unlist(strsplit(inputString, " "))
  idx <- which(splitText != "")
  splitText <- splitText[idx]
  limiter <- 5
  if(length(splitText) >= 2)
  {
    inputString <- paste(splitText[length(splitText) - 1], splitText[length(splitText)], sep = " ")
    ## Check the trigram.
    trigramCandidates <- dplyr::filter(threeWordWithStopWords, bigramIdentifier == inputString)
    if(nrow(trigramCandidates) > 0)
    {
      if(nrow(trigramCandidates) > limiter)
      {
        trigramCandidates <- trigramCandidates[1:limiter,]
      }
      probabilityVector <- sapply(trigramCandidates$term, probabilityCalculationTrigram, oneWordWithStopWords, twoWordWithStopWords, threeWordWithStopWords, inputString, trigramCandidates)
      resultDataFrame <- tbl_df(data.frame(term = names(probabilityVector), probability = probabilityVector, row.names = NULL, stringsAsFactors = FALSE))
      resultDataFrame <- dplyr::arrange(resultDataFrame, desc(probability))
      for(i in 1:nrow(resultDataFrame))
      {
        splitText <- unlist(strsplit(resultDataFrame$term[i], " "))
        resultDataFrame$term[i] <- splitText[length(splitText)]
      }
      return(resultDataFrame)
    }
    ## Check the bigrams.
    inputString <- splitText[length(splitText)]
    bigramCandidates <- dplyr::filter(twoWordWithStopWords, unigramIdentifier == inputString)
    if(nrow(bigramCandidates) > 0)
    {
      if(nrow(bigramCandidates) > limiter)
      {
        bigramCandidates <- bigramCandidates[1:limiter,]
      }
      probabilityVector <- sapply(bigramCandidates$term, probabilityCalculationBigram, oneWordWithStopWords, twoWordWithStopWords, inputString, bigramCandidates)
      resultDataFrame <- tbl_df(data.frame(term = names(probabilityVector), probability = probabilityVector, row.names = NULL, stringsAsFactors = FALSE))
      resultDataFrame <- dplyr::arrange(resultDataFrame, desc(probability))
      for(i in 1:nrow(resultDataFrame))
      {
        splitText <- unlist(strsplit(resultDataFrame$term[i], " "))
        resultDataFrame$term[i] <- splitText[length(splitText)]
      }
      return(resultDataFrame)
    }
    else
    {
      unigramCandidates <- oneWordWithStopWords
      if(nrow(unigramCandidates) > 0)
      {
        if(nrow(unigramCandidates) > limiter)
        {
          unigramCandidates <- unigramCandidates[1:limiter,]
        }
        probabilityVector <- sapply(unigramCandidates$term, probabilityCalculationUnigram, twoWordWithStopWords)
        resultDataFrame <- tbl_df(data.frame(term = names(probabilityVector), probability = probabilityVector, row.names = NULL, stringsAsFactors = FALSE))
        resultDataFrame <- dplyr::arrange(resultDataFrame, desc(probability))
        for(i in 1:nrow(resultDataFrame))
        {
          splitText <- unlist(strsplit(resultDataFrame$term[i], " "))
          resultDataFrame$term[i] <- splitText[length(splitText)]
        }
        return(resultDataFrame)
      }
    }
  }
  else if(length(splitText) == 1)
  {
    inputString <- splitText[length(splitText)]
    bigramCandidates <- dplyr::filter(twoWordWithStopWords, unigramIdentifier == inputString)
    if(nrow(bigramCandidates) > 0)
    {
      if(nrow(bigramCandidates) > limiter)
      {
        bigramCandidates <- bigramCandidates[1:limiter,]
      }
      probabilityVector <- sapply(bigramCandidates$term, probabilityCalculationBigram, oneWordWithStopWords, twoWordWithStopWords, inputString, bigramCandidates)
      resultDataFrame <- tbl_df(data.frame(term = names(probabilityVector), probability = probabilityVector, row.names = NULL, stringsAsFactors = FALSE))
      resultDataFrame <- dplyr::arrange(resultDataFrame, desc(probability))
      for(i in 1:nrow(resultDataFrame))
      {
        splitText <- unlist(strsplit(resultDataFrame$term[i], " "))
        resultDataFrame$term[i] <- splitText[length(splitText)]
      }
      return(resultDataFrame)
    }
    else
    {
      unigramCandidates <- oneWordWithStopWords
      if(nrow(unigramCandidates) > 0)
      {
        if(nrow(unigramCandidates) > limiter)
        {
          unigramCandidates <- unigramCandidates[1:limiter,]
        }
        probabilityVector <- sapply(unigramCandidates$term, probabilityCalculationUnigram, twoWordWithStopWords)
        resultDataFrame <- tbl_df(data.frame(term = names(probabilityVector), probability = probabilityVector, row.names = NULL, stringsAsFactors = FALSE))
        resultDataFrame <- dplyr::arrange(resultDataFrame, desc(probability))
        for(i in 1:nrow(resultDataFrame))
        {
          splitText <- unlist(strsplit(resultDataFrame$term[i], " "))
          resultDataFrame$term[i] <- splitText[length(splitText)]
        }
        return(resultDataFrame)
      }
    }
  }
}

## Trigram model based calculation.
probabilityCalculationTrigram <- function(termOfInterest, oneWordWithStopWords, twoWordWithStopWords, threeWordWithStopWords, inputString, trigramCandidates)
{
  oneOccurenceNgram <- nrow(dplyr::filter(threeWordWithStopWords, freq == 1))
  twoOccurenceNgram <- nrow(dplyr::filter(threeWordWithStopWords, freq == 2))
  discountValueTrigram <- oneOccurenceNgram / (oneOccurenceNgram + (2 * twoOccurenceNgram))
  if(is.na(discountValueTrigram) == TRUE)
  {
    discountValueTrigram <- 0
  }
  bigramPreCount <- dplyr::filter(twoWordWithStopWords, term == inputString)$freq
  bigramPostCount <- nrow(dplyr::filter(trigramCandidates, freq >= 1))
  oneOccurenceNgram <- nrow(dplyr::filter(twoWordWithStopWords, freq == 1))
  twoOccurenceNgram <- nrow(dplyr::filter(twoWordWithStopWords, freq == 2))
  discountValueBigram <- oneOccurenceNgram / (oneOccurenceNgram + (2 * twoOccurenceNgram))
  if(is.na(discountValueBigram) == TRUE)
  {
    discountValueBigram <- 0
  }
  temp <- dplyr::filter(trigramCandidates, term == termOfInterest) %>% select(freq)
  firstTermTrigram <- max(temp - discountValueTrigram, 0) / bigramPreCount
  secondTermTrigram <- discountValueTrigram * bigramPostCount / bigramPreCount
  temp <- dplyr::filter(trigramCandidates, term == termOfInterest)
  bigramCandidates <- dplyr::filter(twoWordWithStopWords, term == temp$bigramPost)
  unigramPreCount <- dplyr::filter(oneWordWithStopWords, term == bigramCandidates$unigramIdentifier[1])$freq
  unigramPostCount <- nrow(dplyr::filter(bigramCandidates, freq >= 1))
  firstTermBigram <- max(bigramCandidates$freq[1] - discountValueBigram, 0) / unigramPreCount
  secondTermBigram <- discountValueBigram * (unigramPostCount / unigramPreCount)
  unigramProbability <- nrow(dplyr::filter(twoWordWithStopWords, unigramPost == bigramCandidates$unigramPost[1]) %>% dplyr::filter(freq >= 1)) /
    nrow(dplyr::filter(twoWordWithStopWords, freq >= 1))
  secondTermBigram <- secondTermBigram * unigramProbability
  bigramProbability <- firstTermBigram + secondTermBigram
  secondTermTrigram <- secondTermTrigram * bigramProbability
  trigramProbability <- firstTermTrigram + secondTermTrigram
  if(length(trigramProbability) == 0)
  {
    trigramProbability <- 0
  }
  trigramProbability
}

## Bigram model based calculation.
probabilityCalculationBigram <- function(termOfInterest, oneWordWithStopWords, twoWordWithStopWords, inputString, bigramCandidates)
{
  oneOccurenceNgram <- nrow(dplyr::filter(twoWordWithStopWords, freq == 1))
  twoOccurenceNgram <- nrow(dplyr::filter(twoWordWithStopWords, freq == 2))
  discountValueBigram <- oneOccurenceNgram / (oneOccurenceNgram + (2 * twoOccurenceNgram))
  if(is.na(discountValueBigram) == TRUE)
  {
    discountValueBigram <- 0
  }
  unigramPreCount <- dplyr::filter(oneWordWithStopWords, term == inputString)$freq
  unigramPostCount <- nrow(dplyr::filter(bigramCandidates, freq >= 1))
  temp <- dplyr::filter(bigramCandidates, term == termOfInterest)
  firstTermBigram <- max(temp$freq - discountValueBigram, 0) / unigramPreCount
  secondTermBigram <- discountValueBigram * (unigramPostCount / unigramPreCount)
  unigramProbability <- nrow(dplyr::filter(twoWordWithStopWords, unigramPost == temp$unigramPost) %>% dplyr::filter(freq >= 1)) /
    nrow(dplyr::filter(twoWordWithStopWords, freq >= 1))
  secondTermBigram <- secondTermBigram * unigramProbability
  bigramProbability <- firstTermBigram + secondTermBigram
  if(length(bigramProbability) == 0)
  {
    bigramProbability <- 0
  }
  bigramProbability
}

## Unigram model based calculation.
probabilityCalculationUnigram <- function(termOfInterest,twoWordWithStopWords)
{
  unigramProbability <- nrow(dplyr::filter(twoWordWithStopWords, unigramPost == termOfInterest) %>% dplyr::filter(freq >= 1)) /
    nrow(dplyr::filter(twoWordWithStopWords, freq >= 1))
  if(length(unigramProbability) == 0)
  {
    unigramProbability <- 0
  }
  unigramProbability
}
