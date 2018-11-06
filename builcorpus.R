buildcorpus <- function(dir, class) {
  filelist <- list.files(dir)
  for (i in 1:length(filelist)){
    path <- paste0(dir, filelist[i])
    tmp <- readLines(path)
    tmp <- str_c(tmp, collapse = "")
    tmp <- str_replace_all(tmp, pattern="<.*?>", replacement = " ")
    tmp <- str_replace_all(tmp, pattern="\\=", replacement = "")
    
    if (!exists("corp")) {
      corp <- VCorpus(VectorSource(tmp))
    } else {
      corp <- c(corp, VCorpus(VectorSource(tmp)))
    }
    meta(corp[[i]], "class") <- class
  }
  corp
}
