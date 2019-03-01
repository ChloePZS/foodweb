setwd("D:/Work, Courses/EPHE/Stage_Coral reef food web CRIOBE/Biblio")
dir()

install.packages("pdftools")
library("pdftools")

#Extract raw text from pdf files

txt<-pdf_text("Randall - FOOD HABITS OF REEF FISHES OF THE WEST INDIES.pdf")
cat(txt[1])
cat(txt[2]) 

txt2 <- strsplit(txt, "\r\n") #\n to show the line breaks and \r to show retour à la ligne
head(txt2[[6]]) #One list for each page. create a list of 94 elements, one for each page
help(strsplit)

#Get columns together
library(stringr)
txt_split <- strsplit(txt, "  ") #split each row when there are 2 spaces. 
help(strsplit)
head(txt_split[6])

txt_split <- lapply(txt_split, function(x) {
  # For each element, extract:
  #    - doc1 that is the first column. 
  #    - doc2 that is the second column.
  doc1 <- x[1:8][x[1:8] != ""][1] # The first piece of text that's not empty
  if (is.na(doc1)) doc1 <- ""
  # doc2 takes the next non-empty piece of text
  doc2 <- x[x != ""] 
  if (doc1 != "") doc2 <- doc2[-1]
  if (length(doc2) == 0) doc2 <- ""
  # Sometimes there is more text needed to be extracted. 
  # I try to give it to either doc1 or doc2 depending on the size of it.
  while (sum(nchar(doc2)) > 65) { #nchar to count nb of characters
    doc1 <- paste(doc1, doc2[1], collapse = " ") #collapse to separate the results
    doc2 <- doc2[-1]
  }

  # Clean it before returning it
  doc2 <- paste(doc2, collapse = " ")
  doc1 <- str_trim(doc1) # stringr::str_trim trim the spaces before/after
  doc2 <- str_trim(doc2)
  list(doc1 = doc1, doc2 = doc2)
})

doc1 <- sapply(txt_split, `[[`, 1) # First column
doc2 <- sapply(txt_split, `[[`, 2) # Second column

doc1[2] #DIDN'T WORK ! LINES not per columns !

# Vector of the page breaks coordinates:
pages_rows <- c(0, which(doc1 == "page"), length(doc1))
doc <- c()
# Page by page, we fill a new vector:
for (i in 1:(length(pages_rows) - 1)) {
  doc <- c(doc, c(doc1[(pages_rows[i] + 1):pages_rows[i + 1]],
                  doc2[(pages_rows[i] + 1):pages_rows[i + 1]]))
}
doc <- doc[doc != "page"] #Clean vector of all txt lines in order, with return to lines 
head(doc)
doc[2]
doc_split<-strsplit(doc, "\r\n") 
doc_split[2]  #Nope didn't work ! But aligned lines no matter of the columns

#Extract the information
help(grep)

#Example
txt <- c("arm","foot","lefroo", "bafoobar")
if(length(i <- grep("foo", txt)))
  cat("'foo' appears at least once in\n\t", txt, "\n")
i # 2 and 4
txt[i]




