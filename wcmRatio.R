
word_db <- read.csv('/Users/lindsaygreene/Desktop/programming/wcmRatio/UNCCombWordDB.csv', na.strings=c("", "NA"))
data_path <- file.path("", "Users", "lindsaygreene", "Desktop")

# set up data frame to store average results  
data <- data.frame(matrix(vector(), ncol=4, nrow=length(files)))  # data frame to store avg output  
files <- list.files(path=data_path, pattern="*.csv")
header_names <- list("Avg_Target_Score","Avg_Actual_Score","Avg_WF_Score")  # column headers for avg output df 
colnames(data) <- header_names
rownames(data) <- files

# set up data frame to store word by word results 
word_by_word <- data.frame(matrix(vector(), ncol=6))  # data frame to store info ab individual words from each transcript
names <- list("File_Name", "Target_Word", "Actual_Production", "Target_WCM", "Actual_WCM","Word_Frequency")  # column headers for word by word df 
colnames(word_by_word) <- names
wbw_row = 1  # count number of rows in word by word db 

for(file in 1:length(files)) {
  
  fileName <- files[file]
  filePath <- paste(data_path, "/", fileName, sep="")  # update file name to absolute path 
  transcript <- read.csv(filePath, na.strings=c("", "NA"))  # read in csv and handle NA values 
  
  
  
}

