markedCalculateWCM<- function(klattese) {  # used when stress and syllables are marked in the sample
  # phoneme categories 
  engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
  engl_voiced_cons <- c("b","d","D","F","g","J","l","M","m","N","n","G","r","v","w","y","z","Z")  # word final M and N? 
  engl_syll_cons <- c("L", "M", "N", "R") 
  engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
  engl_affricates <- c("C","J")
  engl_velars <- c("k","g","G")
  engl_liquids <- c("l","L","r","R","X")
  engl_vowels <- c("i","I","E","e","@","a","W","Y","^","c","O","o","U","u","R","x","|","X","L","M","N")
  
  phon_points <- 0 
  syllables <- 1
  nonInitPrimStress <- 0
  
  # if the word ends in a consonant 
  len <- str_length(klattese)
  final_phoneme <- substr(klattese, len, len)
  if (final_phoneme %in% engl_voiced_cons | final_phoneme %in% engl_voiceless_cons | final_phoneme %in% engl_syll_cons) { 
    phon_points=phon_points+1  # syllable structures (1)
  } 
  
  # if the word has consonant clusters 
  split <- strsplit(klattese, "([iIEe@aWY^cOoUuRx|XLMN\\ˈˌˌ]+|-+)+")  # regular expression to isolate consonants 
  for(i in 1:length(split[[1]])) {
    if(str_length(split[[1]][i]) > 1) { 
      phon_points = phon_points + 1  # syllable structures (2)
    }
  }
  
  # for loop to assign points for sound classes, and find stress and syllables 
  for (i in 1:str_length(klattese)) {
    phoneme <- substr(klattese, i, i)
    
    if(phoneme == '-') syllables=syllables+1

        if(phoneme == 'ˈ' && syllables >= 2) nonInitPrimStress = 1
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
  # WCM rules for word patterns 
  if (syllables > 2) phon_points=phon_points+1  # word patterns (1)
  if (nonInitPrimStress == 1) phon_points=phon_points+1  # word patterns (2)
  
  return(phon_points) 
}

unmarkedCalculateWCM <- function(klattese) {  # used when stress and syllables are not marked in the sample

  engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
  engl_voiced_cons <- c("b","d","D","F","g","J","l","M","m","N","n","G","r","v","w","y","z","Z")  # word final M and N? 
  engl_syll_cons <- c("L", "M", "N", "R") 
  engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
  engl_affricates <- c("C","J")
  engl_velars <- c("k","g","G")
  engl_liquids <- c("l","L","r","R","X")
  engl_vowels <- c("i","I","E","e","@","a","W","Y","^","c","O","o","U","u","R","x","|","X","L","M","N","R")
  
  phon_points <- 0 
  vowels <- 0
  
  # if the word ends in a consonant 
  len <- str_length(klattese)
  final_phoneme <- substr(klattese, len, len)
  if (final_phoneme %in% engl_voiced_cons | final_phoneme %in% engl_voiceless_cons) { 
    phon_points=phon_points+1  # syllable structures (1)
  } 
  
  # if the word has consonant clusters 
  # ** note this section was originally left out of the unmarked WCM 
  # calculation code is borrowed from privWCMratio/markedCalculateWCM function
  
  split <- strsplit(klattese, "([iIEe@aWY^cOoUuRx|XLMNR\\ˈˌˌ]+|-+)+")  # regular expression to isolate consonants
  for(i in 1:length(split[[1]])) {
    if(str_length(split[[1]][i]) > 1) {
      phon_points = phon_points + 1  # syllable structures (2)
    }
  }
  
  
  # for loop to assign points for sound classes, and find stress and syllables 
  for (i in 1:str_length(klattese)) {
    phoneme <- substr(klattese, i, i)
    
    # we need number of vowels to calculate numSyllables with no syllable marking
    if(phoneme %in% engl_vowels) vowels = vowels + 1
    
    # WCM rules for sound classes 
    if (phoneme %in% engl_velars) phon_points=phon_points+1  # sound classes (1)- velars
    if (phoneme %in% engl_liquids) phon_points=phon_points+1  # sound classes (2)- liquids
    if (phoneme %in% engl_fricatives | phoneme %in% engl_affricates) { 
      phon_points=phon_points+1  # sound classes (3)- fricatives or affricates
      if (phoneme %in% engl_voiced_cons) { #
        phon_points=phon_points+1  # sound classes (4)- voiced fric or affric.
      }
    }
  }
  
  # # WCM rules for word patterns 
  if(vowels > 2) phon_points = phon_points + 1  # word patterns (1)
  
  return(phon_points) 
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

correctStress <- function(word) {
  if(grepl("'", word, fixed=TRUE) == TRUE) {  # If apostrophe was used as stress marker in input
    word <- gsub("'", "ˈ", word)  # Replace it with true klattese stress marker 
  }
  return(word)
}
