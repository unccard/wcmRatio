library(tidyr)
library(tidytext)
library(stringr)
library(dplyr)
library(stringdist)
source("functions.R")

# phoneme categories 
engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
engl_voiced_cons <- c("b","d","D","F","g","J","l","M","m","N","n","G","r","v","w","y","z","Z")  # word final M and N? 
# engl_syll_cons <- c("L", "M", "N", "R") 
engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
engl_affricates <- c("C","J")
engl_velars <- c("k","g","G")
engl_liquids <- c("l","L","r","R","X")

word_db <- read.csv('UNCWordDB-2021-10-08.csv', na.strings=c("", "NA"))
tibbletest <-tibble(word_db$KlatteseSyll, word_db$KlatteseBare, word_db$Zipf.value) # isolate the categories we need from word_db

data_path <- file.path("", "Users", "lindsaygreene", "Desktop", "programming", "wcmRatio")
isMarked <- 1  # 1 if word stress & syllabification are coded in transcript, 0 if not (default)

# create list of input files we want to analyze 
files <- list.files(path=data_path, pattern="*-input.csv")

# set up data frame to store average results  
data <- data.frame(matrix(vector(), ncol=6, nrow=length(files)))  # data frame to store avg output 
header_names <- list("Avg_Target_WCM","Avg_Production_WCM", "Avg_WCM_Ratio",
                     "Avg_Error_Rate", "Avg_Accuracy_Rate", "Avg_WF")  # column headers for avg output df 
colnames(data) <- header_names
rownames(data) <- files

# set up data frame to store word by word results 
word_by_word <- data.frame(matrix(vector(), ncol=9))  # data frame to store info ab individual words from each transcript
names <- list("File_Name", "Target", "Production", "Target_WCM","Prod_WCM", "WCM_Ratio", 
              "Phonemic_Error_Rate", "Phonemic_Accuracy_Rate","Word_Frequency")  # column headers for word by word df 
colnames(word_by_word) <- names

for(file in files) {
  filePath <- paste(data_path, "/", file, sep="")  # update file name to absolute path 
  transcript <- read.csv(filePath, na.strings=c("", "NA"))  # read in csv and handle NA values 
  
  # # initialize vectors that will be populated with data for each word in sample 
  # phonetic_tscript <- phonetic_plain_tscript <- wf_tscript <- c()
  # 
  # # populate vectors with data for each word in the transcript 
  # for(i in 1:nrow(transcript)) {
  #   word <- toString(transcript[i,1])
  #   row <- 0
  #   if(!isMarked) row = which(tibbletest[,1] == word)
  #   else row = which(tibbletest[,2] == word)
  #   if(!identical(toString(tibbletest[row, 2]),"character(0)")){  # omit words not found in word_db
  #     phonetic_tscript <- append(phonetic_tscript, toString(tibbletest[row, 1]))
  #     phonetic_plain_tscript <- append(phonetic_plain_tscript, toString(tibbletest[row,2]))
  #     wf_tscript <- append(wf_tscript, toString(tibbletest[row,3]))
  #   }
  # }
  # 
  # # transform the vectors into data frames 
  # phonetic_tscript<-as.data.frame(phonetic_tscript)
  # phonetic_plain_tscript<-as.data.frame(phonetic_plain_tscript)
  # wf_tscript<-as.data.frame(wf_tscript)
  
  # initialize cumulative points for each file 
  target_phon_total <- prod_phon_total <- edit_distance_total <- target_segments_total <- wf_total <- 0 
  
  for(word in 1:nrow(transcript)) {
    target <- prod <- target_plain <- prod_plain <- ""
    target_wcm <- prod_wcm <- wf <- row <- 0
    
    if(!isMarked) {
      row = which(tibbletest[,2] == word)
      # IF FOUND 
      target = toString(tibbletest[row,2])
      prod = transcript[which(transcript[,1] == target), 2]
      target_plain = toString(tibbletest[row,2])
      prod_plain = prod
      target_wcm = unmarkedCalculateWCM(target)
      prod_wcm = unmarkedCalculateWCM(prod)
      wf = toDouble(tibbletest[row,3])
      # ELSE FILL ROW WITH NA 
    } else {
      row = which(tibbletest[,1] == word)
      # IF FOUND 
      target = toString(tibbletest[row,1])
      prod = transcript[which(transcript[,1] == target), 2]
      target_plain = toString(tibbletest[row,2])
      prod_plain = removeMarkers(prod)
      target_wcm = markedCalculateWCM(target)
      prod_wcm = markedCalculateWCM(prod)
      wf = toDouble(tibbletest[row,3])
      # ELSE FILL ROW WITH NA 
    }
    
    # CONDITIONAL FOR IF NA 
    wcm_ratio <- prod_wcm/target_wcm  # calculate ratio of WCM scores 
    lev_dist <- stringdist(prod, target, method="lv")  # calculate Levenshtein distance
    target_segments <- str_length(target_plain)
    phonemic_error_rate <- lev_dist/target_segments
    phonemic_accuracy_rate <- 1 - phonemic_error_rate
    
    # calculate & store info in word by word output 
    word_by_word[word, 1] = fileName
    word_by_word[word, 2] = target_plain
    word_by_word[word, 3] = prod_plain
    word_by_word[word, 4] = target_wcm
    word_by_word[word, 5] = prod_wcm
    word_by_word[word, 6] = wcm_ratio
    word_by_word[word, 7] = phonemic_error_rate
    word_by_word[word, 8] = phonemic_accuracy_rate
    word_by_word[word, 9] = wf
    
    # CONDITIONAL FOR IF NA 
    # add points for current word to cumulative total 
    target_phon_total = target_phon_total + target_wcm
    prod_phon_total = prod_phon_total + prod_wcm
    edit_distance_total = edit_distance_total + lev_dist
    target_segments_total = target_segments_total + target_segments
    wf_total = wf_total + wf
  }
  
  # CONDITIONAL FOR IF NA 
  # calculate averages for file from total points 
  avg_target_wcm <- target_phon_total/nrow(transcript)
  avg_prod_wcm <- prod_phon_total/nrow(transcript)
  avg_phonemic_error_rate <- edit_distance_total/target_segments_total
  avg_phonemic_accuracy_rate <- 1 - avg_phonemic_error_rate
  avg_wf <- wf_total/nrow(transcript)
  
  # write output and file name to avg output data frame  
  data[file, 1] = avg_target_wcm
  data[file, 2] = avg_prod_wcm
  data[file, 3] = avg_wcm_ratio
  data[file, 4] = avg_phonemic_error_rate
  data[file, 5] = avg_phonemic_accuracy_rate
  data[file, 6] = avg_wf
}

# write output to file and save to same location as input files
write.csv(data, file=paste(data_path, "/", "wcmRatio_output.csv", sep=""))
write.csv(word_by_word, file=paste(data_path, "/", "wcmRatio_word_by_word.csv", sep=""))
