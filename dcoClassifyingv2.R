
#Libraries neede
if (!require(stringr)) install.packages('stringr')
if (!require(tm.plugin.webmining)) install.packages('tm.plugin.webmining')
if (!require(tm)) install.packages('tm')
if (!require(SnowballC)) install.packages('SnowballC')
if (!require(RTextTools)) install.packages('RTextTools')
if (!require(R.utils)) install.packages('R.utils')
if (!require(utils)) install.packages('utils')
if (!require('caret')) install.packages('caret')




#######################Remove Unnecessary Files
# identify extraneous ham file and delete
remove_ham <- list.files(path="easy_ham_2/", full.names=T, recursive=FALSE, pattern="cmds")
file.remove(remove_ham)
# identify extraneous spam file and delete
remove_spam <-list.files(path="spam/", full.names=T, recursive=FALSE, pattern="0000.7b1b73cf36cf9dbc3d64e3f2ee2b91f1")
file.remove(remove_spam)


#File List and Name Shuffle
# list of spam files
spam_files <-list.files(path="spam/", full.names=T, recursive=FALSE)
# list of ham files
ham_files <- list.files(path="easy_ham_2/",full.names=T, recursive=FALSE)
# concatenate ham and spam file lists
ham_spam <- c(ham_files,spam_files)
#shuffle file names
set.seed(2020)
ham_spam <- sample(ham_spam,length(ham_spam))


# is required for extractHTMLStrip() function to works.
Sys.setlocale("LC_ALL", "C")
#--------------------------------------


#Cleaning Scripts
# function to find first blank line in email.
# using this function to estimate where email body begins.
find_blank_line <-function(x){
  for (i in 1:length(x)){
    if (str_detect(x[i],"^[:space:]*$")){
      result <- i
      return(i) 
    }
  }
}

# set up variables for loop
n <- 0
if(exists('email_corpus')){rm(email_corpus)} 

# loop through each email
for (i in 1:length(ham_spam)){
  tmp <- readLines(ham_spam[i])
  
  # remove email header
  beg <- find_blank_line(tmp)+1
  end <- length(tmp)
  tmp <- tmp[beg:end]
  
  # remove HTML tags
  if(extractHTMLStrip(tmp)!=""){
    tmp <- extractHTMLStrip(tmp)
  }
  
  # remove URL links, punctuation, numbers, newlines, and misc symbols
  tmp <- unlist(str_replace_all(tmp,"[[:punct:]]|[[:digit:]]|http\\S+\\s*|\\n|<|>|=|_|-|#|\\$|\\|"," "))
  
  # remove extra whitespace
  tmp <- str_trim(unlist(str_replace_all(tmp,"\\s+"," ")))                           
  
  tmp <- str_c(tmp,collapse="")
  
  # Add emails to corpus, and include spam/ham category information
  if (length(tmp)!=0){
    n <- n + 1
    tmp_corpus <- VCorpus(VectorSource(tmp))
    ifelse(!exists('email_corpus'), email_corpus <- tmp_corpus, email_corpus <- c(email_corpus,tmp_corpus))
    meta(email_corpus[[n]], "spam_ham") <- ifelse(str_detect(ham_spam[i],"spam"),1,0)
    
  }
}    

saveRDS(email_corpus,"email_corpus")
#Corpus Scubbing
#Initial DTM
dtm <- DocumentTermMatrix(email_corpus)
#Intermediate DTM
# transform all words in corpus to lower case
email_corpus_mod <- tm_map(email_corpus, content_transformer(tolower))

# remove all stop words (e.g. "i", "me", "she", etc.)
email_corpus_mod <- tm_map(email_corpus_mod,removeWords, words = stopwords("en"))

# stem words: cut certain terms down to word root
email_corpus_mod <- tm_map(email_corpus_mod, stemDocument)

dtm <- DocumentTermMatrix(email_corpus_mod)
dtm
#Final DTM
dtm <- removeSparseTerms(dtm,1-(10/length(email_corpus_mod)))
dtm
saveRDS(dtm,"DTM_Final")

###Classifier Models
# create spam label vector for each email which indiciates actual status of "spam" or "not spam" 

spam_labels_prelim <- unlist(meta(email_corpus_mod,"spam_ham"))

spam_labels <- c(rep(NA,length(email_corpus_mod)))

for (i in 1:length(email_corpus_mod)){
  spam_labels[i] <- spam_labels_prelim[[i]]
}


#Set Up Models
# number of emails in corpus
N <- length(spam_labels)
trainpartition <- round(.80 * N)
# set up model container; 80/20 split between train and test data
container <- create_container(
  dtm,
  labels = spam_labels,
  trainSize = 1:trainpartition,
  testSize = (trainpartition+1):N,
  virgin = FALSE
)

svm_model <- train_model(container, "SVM")
tree_model <- train_model(container, "TREE")
maxent_model <- train_model(container, "MAXENT")
glm_model <- train_model(container, "GLMNET")

svm_out <- classify_model(container, svm_model)
tree_out <- classify_model(container, tree_model)
maxent_out <- classify_model(container, maxent_model)
glm_out <- classify_model(container,glm_model)


########################################Model Performance
# create lables:  actual classification, then model classification
# for three models on test data

labels_out <- data.frame(
  correct_label = spam_labels[(trainpartition+1):N],
  svm = as.character(svm_out[,1]),
  tree = as.character(tree_out[,1]),
  maxent = as.character(maxent_out[,1]),
  glm = as.character(glm_out[,1]),
  stringAsFactors = F)

#func to call confusionMatrix 
makematrix <- function(col) {
  confusionMatrix(table(labels_out[[col]], labels_out$correct_label), positive= "0")
}

# create all 
svmconf <- makematrix('svm')
treeconf <- makematrix('tree')
glmconf <- makematrix('glm')
maxentconf <- makematrix('maxent')

#Plot: Confusion Matrix
par(mfrow=c(2,2))
fourfoldplot(svmconf$table, color = c("#B22222", "#2E8B57"), main="SVM")
fourfoldplot(treeconf$table, color = c("#B22222", "#2E8B57"), main="Tree")
fourfoldplot(glmconf$table, color = c("#B22222", "#2E8B57"), main="GLM")
fourfoldplot(maxentconf$table, color = c("#B22222", "#2E8B57"), main="MaxEnt")


#####################Comparison Table: F1 Score
#setup df
eval <- data.frame(treeconf$byClass, 
                   svmconf$byClass,
                   glmconf$byClass,
                   maxentconf$byClass)

eval <- data.frame(t(eval))

#calc FScore
precision <- eval$Pos.Pred.Value
recall <- eval$Sensitivity
eval$Fscore <- 2 * ((precision * recall) / (precision + recall))  

# manipulate results DF
eval <- eval[,c(1:3,9,12)]
row.names(eval) <- c("Tree", "SVM", "GLM", "MaxEnt")
eval <- eval[order(eval$Fscore, decreasing=TRUE),]

knitr::kable(eval)



classify_doc <- function(doc, train_dtm,model)
{
 doc_dtm<- c(create_matrix(doc),train_dtm)
  predict_container <- create_container(doc_dtm, NULL, testSize = 1, virgin = FALSE) # testSize = 1 - we have only one row!
  return(classify_model(predict_container, model))
 #doc_dtm
}

#########################################
prep_doc <- function(tmp){

  # remove email header
  beg <- find_blank_line(tmp)+1
  end <- length(tmp)
  tmp <- tmp[beg:end]
  
  # remove HTML tags
  if(extractHTMLStrip(tmp)!=""){
    tmp <- extractHTMLStrip(tmp)
  }
  
  # remove URL links, punctuation, numbers, newlines, and misc symbols
  tmp <- unlist(str_replace_all(tmp,"[[:punct:]]|[[:digit:]]|http\\S+\\s*|\\n|<|>|=|_|-|#|\\$|\\|"," "))
  
  # remove extra whitespace
  tmp <- str_trim(unlist(str_replace_all(tmp,"\\s+"," ")))                           
  
  tmp <- str_c(tmp,collapse="")
  tmp
}    


docList <- list.files("To_build/", full.names=T, recursive=FALSE)

  

Labels <- function(doc_to_label, doc_dtm, model)
{
  #matTmp <- VCorpus(VectorSource(docs_to_label))
  predict_matrix <- create_matrix(doc_to_label, originalMatrix = doc_dtm)
  predict_container <- create_container(predict_matrix, labels = NULL, trainSize = NULL ,testSize = 1, virgin = FALSE) 
  return(classify_model(predict_container, model))
}


#####################################################################
predict_courpus <- predict_corpus_build("~/To_label/")
predictions <- Labels(predict_corpus,dtm,svm_model)