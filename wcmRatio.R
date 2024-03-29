library(tidyr)
library(tidytext)
library(stringr)
library(dplyr)
library(stringdist)
source("functions.R")

word_db <- read.csv('UNCWordDB-2022-02-07.csv', na.strings=c("", "NA"))
tibbletest <-tibble(word_db$KlatteseSyll, word_db$KlatteseBare, word_db$Zipf.value) # isolate the categories we need from word_db

#data_path <- file.path("", "Users", "lindsaygreene", "Desktop", "programming", "wcmRatio")
#data_path <- "/Users/jacksa/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - CARD/Variability/REPEAT data/FINALcsvfilesVAR/"

data_path <- "/Users/jacksa/Documents/github/wcmRatio"

isMarked <- 0  # 1 if word stress & syllabification are coded in transcript, 0 if not (default)

# create list of input files we want to analyze 
#files <- list.files(path=data_path, pattern="*-input.csv")
files <- list.files(path=data_path, pattern="*.csv")

files_to_exclude <- c("wcmRatio_output.csv", "wcmRatio_word_by_word.csv", "UNCWordDB-2022-02-07.csv")
files_clean <- files[! files %in% files_to_exclude]
all_files <- files
files <- files_clean

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
wbw_row <- 1  # track row in wbw output - necessary for when we have more than one file 

for(file in files) {
  filePath <- paste(data_path, "/", file, sep="")  # update file name to absolute path 
  transcript <- read.csv(filePath, na.strings=c("", "NA"))  # read in csv and handle NA values 

  # initialize cumulative points for each file 
  target_phon_total <- prod_phon_total <- edit_distance_total <- target_segments_total <- wf_total <- 0 
  
  for(word in 1:nrow(transcript)) {
    target <- prod <- target_plain <- prod_plain <- ""
    target_wcm <- prod_wcm <- wf <- row <- 0
    
    
    # this section is for transcriptions that are not marked for syllable 
    # boundary or stress
    if(isMarked == 0) {
      transcript[word, 1] <- removeMarkers(transcript[word,1])  # Make sure it is really unmarked!
      transcript[word, 2] <- removeMarkers(transcript[word,2])  # production also needs to be unmarked
      
      # if we find the unmarked target word in the database, we can maybe do 
      # something with it. in theory we could use it to get correct syllabi-
      # fication for the target, but that doesn't really help us with the 
      # production.
      
      # But we can use it to get word frequency
      
      row = which(tibbletest[,2] == toString(transcript[word, 1]))  # find unmarked target word in the database- 
      
      #if(length(row) == 0) {  # if the word is not found in the database ## at this time we skip it if not in db...
      #  word_by_word[wbw_row, 1] = file
      #  word_by_word[wbw_row, 2] = toString(transcript[word, 1])
      #  word_by_word[wbw_row, 3] = toString(transcript[word, 2])
      #  wbw_row = wbw_row + 1
      #  next  # keyword to end loop early and start next one 
      #}  # else the word was found in database, so perform calculations 

      #target = toString(tibbletest[row,2]) # this would be finding the standard unmarked target- why?
      #prod = transcript[which(transcript[,1] == target), 2] # this finds the production for the target that matches the current target. why??
      #target_plain = toString(tibbletest[row,2]) # this all seems circular to me. we already created a plain target by removing markers. 
      target <- transcript[word, 1]
      prod <- transcript[word,2]
      target_plain = target
      prod_plain = prod
      target_wcm = unmarkedCalculateWCM(target)
      prod_wcm = unmarkedCalculateWCM(prod)
 
     if(length(row) > 0) {
      wf = as.double(tibbletest[row,3]) # as long as we recognize target, we can get word freq.
      }

      
      # this part for marked transcriptions
            
    } else {
      transcript[word, 1] <- correctStress(transcript[word, 1]) #fix non-standard stress marker
      row = which(tibbletest[,1] == toString(transcript[word, 1]))  # find marked word in the database
    
      # we are not going to skip words not found in the database- we just can't get word freq is all
      #  if(length(row) == 0) {  # if the word is not found in the database
      #  word_by_word[wbw_row, 1] = file
      #  word_by_word[wbw_row, 2] = removeMarkers(toString(transcript[word, 1]))
      #  word_by_word[wbw_row, 3] = removeMarkers(toString(transcript[word, 2]))
      #  wbw_row = wbw_row + 1
      #  next
      #}  # else the word was found, so perform calculations 
     # target = toString(tibbletest[row,1]) # why would we go to the database for the target?
      
      target =  transcript[word, 1]
      #prod = correctStress(transcript[which(transcript[,1] == target), 2])
      prod = correctStress(transcript[word,2])
      target_plain = removeMarkers(target) #toString(tibbletest[row,2])
      prod_plain = removeMarkers(prod)
      target_wcm = markedCalculateWCM(target)
      prod_wcm = markedCalculateWCM(prod)
      wf = as.double(tibbletest[row,3])
    }
    
    wcm_ratio <- prod_wcm/target_wcm  # calculate ratio of WCM scores 
    lev_dist <- stringdist(prod, target, method="lv")  # calculate Levenshtein distance
    target_segments <- str_length(target_plain)  # number of phonemes in the target 
    phonemic_error_rate <- lev_dist/target_segments
    phonemic_accuracy_rate <- 1 - phonemic_error_rate
      
    # calculate & store info in word by word output 
    word_by_word[wbw_row, 1] = file
    
    if (isMarked == 1){
      word_by_word[wbw_row, 2] = target
      word_by_word[wbw_row, 3] = prod
    }  else { # if unmarked, we leave the transcriptions plain
        word_by_word[wbw_row, 2] = target_plain
        word_by_word[wbw_row, 3] = prod_plain
    }
    
    word_by_word[wbw_row, 4] = target_wcm
    word_by_word[wbw_row, 5] = prod_wcm
    word_by_word[wbw_row, 6] = wcm_ratio
    word_by_word[wbw_row, 7] = phonemic_error_rate
    word_by_word[wbw_row, 8] = phonemic_accuracy_rate
    word_by_word[wbw_row, 9] = wf
    
    wbw_row = wbw_row + 1  # increment row in wbw output 
      
    # add points for current word to cumulative total 
    target_phon_total = target_phon_total + target_wcm
    prod_phon_total = prod_phon_total + prod_wcm
    edit_distance_total = edit_distance_total + lev_dist
    target_segments_total = target_segments_total + target_segments
    wf_total = wf_total + wf
  }
  
  # calculate averages for file from total points 
  avg_wcm_ratio <- avg_phonemic_error_rate <- avg_phonemic_accuracy_rate <- 0
  avg_target_wcm <- target_phon_total/nrow(transcript)
  avg_prod_wcm <- prod_phon_total/nrow(transcript)
  if(avg_target_wcm != 0) avg_wcm_ratio = avg_prod_wcm/avg_target_wcm
  if(target_segments_total > 0) avg_phonemic_error_rate = edit_distance_total/target_segments_total
  if(avg_phonemic_error_rate > 0) avg_phonemic_accuracy_rate = 1 - avg_phonemic_error_rate
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
