library(tidyr)
library(tidytext)
library(stringr)
library(dplyr)
library(stringdist)

# HELPER FUNCTIONS 
calculateWCM<- function(klattese, index, hasWordStressCoded) {  # calculate WCM score for the word 
  phon_points <- 0 
  syllables <- 1
  nonInitPrimStress <- 0
  
  # if the word ends in a consonant 
  len <- str_length(klattese)
  final_phoneme <- substr(klattese, len, len)
  if (final_phoneme %in% engl_voiced_cons | final_phoneme %in% engl_voiceless_cons) { 
    phon_points=phon_points+1  # syllable structures (1)
  } 
  
  # if the word has consonant clusters 
  split <- strsplit(klattese, "([iIEe@aWY^cOoUuRx|X\\ˈ]+|-+)+")  # regular expression to isolate consonants 
  for(i in 1:length(split[[1]])) {
    if(str_length(split[[1]][i]) > 1) { 
      phon_points = phon_points + 1  # syllable structures (2)
    }
  }
  
  # for loop to assign points for sound classes, and find stress and syllables 
  for (i in 1:str_length(klattese)) {
    phoneme <- substr(klattese, i, i)
    if(phoneme == '-') syllables=syllables+1
    if(hasWordStressCoded == 1 && phoneme == 'ˈ' && syllables >= 2) nonInitPrimStress = 1
    # WCM rules for sound classes 
    if (phoneme %in% engl_velars) phon_points=phon_points+1  # sound classes (1)
    if (phoneme %in% engl_liquids) phon_points=phon_points+1  # sound classes (2)
    if (phoneme %in% engl_fricatives | phoneme %in% engl_affricates) {
      phon_points=phon_points+1  # sound classes (3)
      if (phoneme %in% engl_voiced_cons) {
        phon_points=phon_points+1  # sound classes (4)
      }
    }
  }
  if (syllables > 2) phon_points=phon_points+1  # word patterns (1)
  if (nonInitPrimStress == 1) phon_points=phon_points+1  # word patterns (2)
  
  return(phon_points) 
}

calculateRatio <- function(prod, target) {
  return(prod/target)
}

removeMarkers <- function(klattese) {  # remove stress and syllable markers for readability
  klattese_plain = ""
  for(i in 1:str_length(klattese)) {
    phoneme <- substr(klattese, i, i)
    if((phoneme >= 41 && phoneme >= 90) || (phoneme >= 61 && phoneme >= 122)) {
      klattese_plain = paste(klattese_plain, phoneme, sep = "")
    } else if(phoneme == '@' || phoneme == '^' || phoneme == '|') {
      klattese_plain = paste(klattese_plain, phoneme, sep = "")
    }
  }
  return(klattese_plain)
}

# phoneme categories 
engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
engl_voiced_cons <- c("b","d","D","F","g","J","l","M","m","N","n","G","r","v","w","y","z","Z")  # word final M and N? 
# engl_syll_cons <- c("L", "M", "N", "R") 
engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
engl_affricates <- c("C","J")
engl_velars <- c("k","g","G")
engl_liquids <- c("l","L","r","R","X")

word_db <- read.csv('/Users/lindsaygreene/Desktop/programming/wcmRatio/UNCCombWordDB.csv', na.strings=c("", "NA"))
data_path <- file.path("", "Users", "lindsaygreene", "Desktop", "programming", "wcmRatio")
hasWordStressCoded <- 1  # 1 if word stress is coded in transcript, 0 if not 

# isolate the categories we need from word_db
tibbletest <-tibble(word_db$phon_klattese, word_db$SUBTLWF0to10)   

# set up data frame to store average results  
data <- data.frame(matrix(vector(), ncol=5, nrow=length(files)))  # data frame to store avg output  
files <- list.files(path=data_path, pattern="*-input.csv")
header_names <- list("Avg_Target_WCM","Avg_Production_WCM", "Avg_WCM_Ratio",
                     "Avg_Edit_Proportion","Avg_WF_Score")  # column headers for avg output df 
colnames(data) <- header_names
rownames(data) <- files

# set up data frame to store word by word results 
word_by_word <- data.frame(matrix(vector(), ncol=8))  # data frame to store info ab individual words from each transcript
names <- list("File_Name", "Target", "Production", "Target_WCM","Prod_WCM",
              "WCM_Ratio", "Edit_Proportion", "Word_Frequency")  # column headers for word by word df 
colnames(word_by_word) <- names
wbw_row = 1  # count number of rows in word by word db 

for(file in 1:length(files)) {
  
  fileName <- files[file]
  filePath <- paste(data_path, "/", fileName, sep="")  # update file name to absolute path 
  transcript <- read.csv(filePath, na.strings=c("", "NA"))  # read in csv and handle NA values 
  
  # initialize vectors to populate with data for each word in sample 
  foundInDB_tscript <- c()  # each target Klattese word that is found in the db
  wf_tscript <- c()  # frequency of each word 
  
  # initialize cumulative points for each file 
  target_phon_total <- prod_phon_total <- edit_distance_total <- target_segments_total <- wf_total <- 0 
  
  # populate vectors with data for each word in the transcript
  for(i in 1:nrow(transcript)) {
    word <- toString(transcript[i,1])
    row <- which(tibbletest[,1] == word)
    if(!identical(toString(tibbletest[row, 1]),"character(0)")) {  # omit words not found in word_db
      foundInDB_tscript <- append(foundInDB_tscript, toString(tibbletest[row, 1]))
      wf_tscript <- append(wf_tscript, toString(tibbletest[row, 2]))
    }
  }
  
  # transform vectors into data frames 
  foundInDB_tscript<-as.data.frame(foundInDB_tscript)
  wf_tscript<-as.data.frame(wf_tscript)
  
  for(word in 1:nrow(foundInDB_tscript)) {
    target <- foundInDB_tscript[word,1]
    prod <- transcript[which(transcript[,1] == target), 2]
    target_plain <- removeMarkers(target)
    prod_plain <- removeMarkers(prod)
    target_wcm <- calculateWCM(target, word, hasWordStressCoded)
    prod_wcm <- calculateWCM(prod, word, hasWordStressCoded)
    wcm_ratio <- calculateRatio(prod_wcm, target_wcm)  # calculate ratio of WCM scores 
    lev_dist <- stringdist(prod, target, method="lv")  # calculate Levenshtein distance
    target_segments <- str_length(target_plain)
    edit_proportion <- lev_dist/target_segments
    wf <- as.double(wf_tscript[word,1])
    
    # calculate & store info in word by word output 
    word_by_word[wbw_row, 1] = fileName
    word_by_word[wbw_row, 2] = target_plain
    word_by_word[wbw_row, 3] = prod_plain
    word_by_word[wbw_row, 4] = target_wcm
    word_by_word[wbw_row, 5] = prod_wcm
    word_by_word[wbw_row, 6] = wcm_ratio
    word_by_word[wbw_row, 7] = edit_proportion
    word_by_word[wbw_row, 8] = wf
    
    wbw_row = wbw_row + 1  # move to next row in the word by word df 
    
    # add points for current word to cumulative total 
    target_phon_total = target_phon_total + target_wcm
    prod_phon_total = prod_phon_total + prod_wcm
    edit_distance_total = edit_distance_total + lev_dist
    target_segments_total = target_segments_total + target_segments
    wf_total = wf_total + wf
  }
  
  # calculate averages for file from total points 
  avg_target_wcm <- target_phon_total/nrow(foundInDB_tscript)
  avg_prod_wcm <- prod_phon_total/nrow(foundInDB_tscript)
  avg_wcm_ratio <- calculateRatio(prod_phon_total, target_phon_total)
  avg_edit_proportion <- edit_distance_total/target_segments_total
  avg_wf <- wf_total/nrow(wf_tscript)
  
  # write output and file name to avg output data frame  
  data[file, 1] = avg_target_wcm
  data[file, 2] = avg_prod_wcm
  data[file, 3] = avg_wcm_ratio
  data[file, 4] = avg_edit_proportion
  data[file, 5] = avg_wf
}

# write output to file and save to same location as input files
write.csv(data, file=paste(data_path, "/", "wcmRatio_output.csv", sep=""))
write.csv(word_by_word, file=paste(data_path, "/", "wcmRatio_word_by_word.csv", sep=""))

