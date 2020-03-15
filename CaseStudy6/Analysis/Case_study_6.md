---
title: 'Team CCAD Case Study 6: Spam Ham, Problem 19'
author: "David Josephs, Andy Heroy, Carson Drake, Che' Cobb"
date: '2020-02-18'
output:
  html_document:
    code_folding: hide
    df_print: paged
    fig_caption: true
    fig_height: 10
    fig_retina: yes
    fig_width: 10
    highlight: haddock
    keep_md: yes
    number_sections: yes
    theme: readable
    toc: yes
  pdf_document:
    toc: yes
---





```r
library(rpart)
library(caret)
library(dplyr)
library(rpart.plot)
library(parallel)
library(doParallel)
library(foreach)
library(iterators)

counter <- function() {
  x <- 0
  return (
          function() {
            x <<- x+1
            return(x)
          }

  )
}

cnt <- counter()

cap <- function(str) {
  paste("**Figure", cnt(), ":**",str)
}
```

# Introduction

Spam email is an ever present annoyance in most of modern day life. This case study is to examine the Spam Assassin dataset (http://spamassassin.apache.org) in which we evaluate 9000 emails with spam filters by classification tree's and recursive partitioning.  To do this, the library `rpart` is utilized and we attempt to improve upon default model classification accuracies by exploring the many parameters available in the rpart.control function.  

We've selected question 19 from the Nolan and Lang book for our analysis and exploration. The question states: 

Consider the other parameters that can be used to control the recursive partitioning process. Read the documentation for them in the rpart.control() documentation. Also, carry out an Internet search for more information on how to tweak the rpart() tuning parameters. Experiment with values for these parameters. Do the trees that result make sense with your understanding of how the parameters are used? Can you improve the prediction using them?

Thus, we will determine how the complexity (meaning depth and number of leaves) of a recursive partitioning tree affects the performance of the model, using various metrics.

# Background

The spam assassin dataset is contrived of 9000 emails each comprised of a header, message body and attachments.  The first goal is to understand the design of the email structure in order to correctly identify what portion of the email is the header, message body and attachments.  After dropping attachments, the headers and message bodies are processed for characteristics that possibly could mark an email as spam.  Looking for characteristics such as capitalization, punctuation or word counts of known spam words that can be found within.  



```r
# we are starting from here, 
# the code used to create the RDA will be shown in the appendix, 
# it seems to run fine on windows but crash on linux
# so this was a good compromise without digging too deeply into the web of 
# lapplys

emailDFrp <- read.csv("https://raw.githubusercontent.com/Landcruiser87/7333_QTW/master/CaseStudy6/Data/data.csv")
emailDFrp$X <- NULL
#load(./Data/data.rda)
emails <- emailDFrp

#str(emails$isSpam)

setupRnum = function(data) {
  logicalVars = which(sapply(data, is.logical))
  facVars = lapply(data[ , logicalVars], 
                   function(x) {
                      x = as.factor(x)
                   })
  cbind(facVars, data[ , - logicalVars])
}

emails = setupRnum(emails)
emails[is.na(emails)]<-0

df_process <- emails
levels(df_process$isSpam) <- c("F","T")
```

# Analysis

To start off the analysis, we've used the methods provided in Nolan and Lang [1] for importing a clean dataset that can be parsed for examples of spam or ham messages and the generated features about each email message.  We're to answer the question of exploring the rpart.control function so we will import the generated dataset from the starter code that was provided by Dr. Slater.  See the appendix for the code that contains all processing functions and setup of the data.

In order to properly quantify the decision tree's performance we now define the metrics we use to evaluate results.  Since this is a classification problem, our metrics will be accuracy, precision, recall and F1-Score. Our goal is to minimize the amount of false positives, and false negatives within the resulting confusion matrix's.  


```r
pander::pander(
 list( Accuracy = "Accuracy is defined as the number of correct predictions divided by the total number of predictions.",
            list("(True Positive + False Negatives)/Total amount of samples"),
       "Precision" = "Precision is defined as the ratio of correctly predicted positives to the total number of predicted positive observations.",
            list("True Positives/(True Positives + False Positives)"),
       "Recall" = "Recall is defined as the ratio of correct positives to all the observations in the class",
            list("True Positives/(True Positives + False Positives)"),
       "F1-Score" = "F1 score is defined as the harmonic average of Precision and Recall. This metric is more useful when you have uneven class balances but it sometimes useful as it includes false postivies and false negatives. ",
            list("2x(Recall x Precision)/(Recall + Position)")
 ))
```



  * **Accuracy**: Accuracy is defined as the number of correct predictions divided by the total number of predictions.
  *

      * (True Positive + False Negatives)/Total amount of samples

  * **Precision**: Precision is defined as the ratio of correctly predicted positives to the total number of predicted positive observations.
  *

      * True Positives/(True Positives + False Positives)

  * **Recall**: Recall is defined as the ratio of correct positives to all the observations in the class
  *

      * True Positives/(True Positives + False Positives)

  * **F1-Score**: F1 score is defined as the harmonic average of Precision and Recall. This metric is more useful when you have uneven class balances but it sometimes useful as it includes false postivies and false negatives.
  *

      * 2x(Recall x Precision)/(Recall + Position)


<!-- end of list -->

Our next step is to explore the recursive partitioning algorithm to pass it various parameters that may improve its classification accuracy.  The parameters that we're going to explore are listed below.  


```r
pander::pander(
 list(
    "cp-complexity" ="This parameter is meant to improve computing efficiency in order to avoid uncessary splits if there isn't an increase in R-squared value.",
    "minsplit" = "Seeing as a tree is the result of the rpart classification.  The minsplit parameter defintes the number of observations necessary in an a parent node in order to quality that branch to be split.  We chose to 3:1 ratio of minsplit to minbucket.",
    "minbucket" = "This parameter controls the lowest number of observations that can exist in an end/terminal node.",
    "maxdepth" = "This parameter defines how deep a final tree can be built by limiting the amount of levels of nodes that can be created.  The help files suggest not going too deep in a tree as it will result in overfitting.  Therefore we won't build a tree greater than 30 levels."
    ))
```



  * **cp-complexity**: This parameter is meant to improve computing efficiency in order to avoid uncessary splits if there isn't an increase in R-squared value.
  * **minsplit**: Seeing as a tree is the result of the rpart classification.  The minsplit parameter defintes the number of observations necessary in an a parent node in order to quality that branch to be split.  We chose to 3:1 ratio of minsplit to minbucket.
  * **minbucket**: This parameter controls the lowest number of observations that can exist in an end/terminal node.
  * **maxdepth**: This parameter defines how deep a final tree can be built by limiting the amount of levels of nodes that can be created.  The help files suggest not going too deep in a tree as it will result in overfitting.  Therefore we won't build a tree greater than 30 levels.

<!-- end of list -->

Now that we've defined some of the available rpart.control functions, its time to start building decision tree's.  To start, we'll show what the decision tree output looks like with the default values for rpart.  Afterwords, we move the values to build extreme cases of the tree size, from very shallow to massive. We use these extremities to infer the relationship between tree size and model performance (as the relationship between parameters and tree size is at this point well known).


## Default parameters decision tree

First, we split the data into stratified train and test samples, using the caret `createDataPartition` function. To see the code, simply click the code button on the right!


```r
set.seed(54)
#Get a count for spam and ham
is_Spam_ct <- table(df_process$isSpam)["T"]
#    T 
# 2397 
is_Ham_ct <- table(df_process$isSpam)["F"]
#    F 
# 6951 

#Sample the indexes
# note createdatapartition is stratified
train_idx <- createDataPartition(df_process$isSpam, p = 0.66, list = F)
spam_train <- df_process[train_idx,]
spam_test <- df_process[-train_idx,]
```

Next, lets fit and plot a standard decision tree. This tree uses a minsplit of 2, and everything else is set to the default value.


```r
#Fitting the regression tree.
rpart_fit = rpart(isSpam ~ ., 
                  data = spam_train,
                  minsplit = 2,
                  method = "class")
#Plot it with either prp or rpart.plot
#prp(rpart_fit, extra = 106)
rpart.plot(rpart_fit)
```

<div class="figure" style="text-align: center">
<img src="Case_study_6_files/figure-html/unnamed-chunk-2-1.svg" alt="**Figure 1 :** A standard decision tree"  />
<p class="caption">**Figure 1 :** A standard decision tree</p>
</div>

As a baseline, lets also check how this tree performs. We will use Accuracy, Precision, Recall, and F1 Score(using spam or "T" as the positive class). Given a confusion matrix of predictions vs truth, $M$, we can get these values in the following manner:

\begin{align}
\mathrm{acc} = \left(\mathop{\sum_{j}\sum_{i}}_{i=j} M\right) / \left(\mathop{\sum_{j}\sum_{i}} M\right)\\
\mathrm{precision} = \mathrm{diag}(M) / \left(\mathop{\sum_{j}} M\right)\\
\mathrm{recall} = \mathrm{diag}(M) / \left(\mathop{\sum_{i}} M\right)\\
\mathrm{f1} = \frac{2(\mathrm{precision})(\mathrm{recall})}{\mathrm{precision} + \mathrm{recall}}
\end{align}

We also count the number of leaves and the depth of a tree. Lets see how this tree did:


```r
#Now lets make some predictions.  We'll predict on the df_test_rpart, with the fit we just achieved directly above. 
rp_predict <- predict(rpart_fit,
                      newdata = spam_test,
                      type = "class")

confusion_mat <- function(preds, truth) {
  return(table(preds,truth))
}
accuracy <- function(preds, truth=spam_test$isSpam) {
  cm <- confusion_mat(preds, truth)
  sum(diag(cm))/sum(cm)
}
precision <- function(preds, truth=spam_test$isSpam) {
  cm <- confusion_mat(preds, truth)
  diag(cm) / colSums(cm)
}
recall <- function(preds, truth=spam_test$isSpam) {
  cm <- confusion_mat(preds, truth)
  diag(cm) / rowSums(cm)
}
f1 <- function(preds, truth=spam_test$isSpam, positive.class = "T") {
  precision <- precision(preds, truth)
  recall <- recall(preds, truth)
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  #Assuming that F1 is zero when it's not possible compute it
  f1[is.na(f1)] <- 0
  #Binary F1 or Multi-class macro-averaged F1
  ifelse(nlevels(truth) == 2, f1[positive.class], mean(f1))
}
metrics <- list("accuracy" = accuracy, "precision" = precision, "recall" = recall, "f1_score" =  f1)
get_metrics <- function(preds, truth = spam_test$isSpam){
  unlist(lapply(metrics, function(f) f(preds)))
}

get_depth <- function(fit) {
  nodes <- as.numeric(rownames(fit$frame))
  max(rpart:::tree.depth(nodes))
}

get_leaves <- function(fit) {
  sum(fit$frame$var=="<leaf>")
}

make_metric_frame <- function(fit, pred) {
  out = c(
          "leaves" = get_leaves(fit),
          "depth" = get_depth(fit),
          get_metrics(pred)
  )
  return(data.frame(t(out)))
}

result_0 <- make_metric_frame(rpart_fit, rp_predict)
result_0
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["leaves"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["depth"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["accuracy"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["precision.F"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["precision.T"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["recall.F"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["recall.T"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["f1_score"],"name":[8],"type":["dbl"],"align":["right"]}],"data":[{"1":"17","2":"10","3":"0.9","4":"0.93","5":"0.83","6":"0.94","7":"0.8","8":"0.81"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

A moderately strong baseline. Lets now fiddle with the tree structure a bit and see what happens.


## Tree Exploration


### Small, Shallow Tree

We expect this to perform significantly worse than the moderately sized tree observed in the previous section. First, lets build the tree. We use a minsplit of 3, cp of 0.01, and minbucket of 2, however all these are really overridden by the maxdepth of 2.

```r
#2nd Tree combination
rpart_tree2 <- rpart(isSpam ~., data=spam_train, 
                     na.action = na.rpart, 
                     method = "class",
                     control=rpart.control(minsplit = 3, 
                                           maxdepth= 2, 
                                           minbucket = 2, 
                                           cp = 0.010)) 

rpart.plot(rpart_tree2)
```

<div class="figure" style="text-align: center">
<img src="Case_study_6_files/figure-html/unnamed-chunk-4-1.svg" alt="**Figure 2 :** A small, short tree"  />
<p class="caption">**Figure 2 :** A small, short tree</p>
</div>

This tree is very small, similar to those used in boosting algorithms (closer to a stump than a tree). It will likely not have very strong predictive power. Lets see!

```r
rp_predict2 <- predict(rpart_tree2,
                      newdata = spam_test,
                      type = "class")


#result_2 <- data.frame(t(c("depth"=get_depth(rpart_tree2),get_metrics(rp_predict2))))
#result_2
result_2 <- make_metric_frame(rpart_tree2, rp_predict2)
result_2
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["leaves"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["depth"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["accuracy"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["precision.F"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["precision.T"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["recall.F"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["recall.T"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["f1_score"],"name":[8],"type":["dbl"],"align":["right"]}],"data":[{"1":"3","2":"2","3":"0.83","4":"0.97","5":"0.43","6":"0.83","7":"0.84","8":"0.57"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Across the board, the small tree performed worse than the reasonably sized tree discussed earlier. It did an especially bad job in the case of the minority class (spam or "T"), with a precision just over 0.56. That means only about 50% of the time the model predicted an email was spam, it was correct. Lets now try a slightly larger tree.

### Moderately Large Tree

We expect this tree to perform better than the all the previous trees. We no longer hinder the tree by depth, setting maxdepth to 30 (the maximum value for the rpart function), a minimum split of 3 samples, a minimum bucket size of 2, and a complexity parameter of 0.005. Lets first check the tree out.


```r
#3rd Tree combination
rpart_tree3 <- rpart(isSpam ~., data=spam_train, 
                     na.action = na.rpart, 
                     method = "class",
                     control=rpart.control(minsplit = 3, 
                                           maxdepth=30, 
                                           minbucket = 2, 
                                           cp = 0.005)) 

rpart.plot(rpart_tree3)
```

<div class="figure" style="text-align: center">
<img src="Case_study_6_files/figure-html/unnamed-chunk-6-1.svg" alt="**Figure 3 :** A moderately large tree"  />
<p class="caption">**Figure 3 :** A moderately large tree</p>
</div>

Lets also check out how this model did. Since it is slightly bigger than the first tree, it should perform slightly better, but by how much is the question.


```r
rp_predict3 <- predict(rpart_tree3,
                      newdata = spam_test,
                      type = "class")


result_3 <- make_metric_frame(rpart_tree3, rp_predict3)
result_3
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["leaves"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["depth"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["accuracy"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["precision.F"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["precision.T"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["recall.F"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["recall.T"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["f1_score"],"name":[8],"type":["dbl"],"align":["right"]}],"data":[{"1":"25","2":"11","3":"0.91","4":"0.93","5":"0.87","6":"0.95","7":"0.8","8":"0.83"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

As expected, the slightly larger tree performed slightly better across the board. 

### Extremely Large Tree

We will now expand our analysis, and attempt to construct the largest tree possible. We use a minsplit of 1 sample, a minbucket of 1, a maximum depth of 30, and a complexity parameter of $1*10^{-7}$. This tree should perform very well, however it is doubtful it will generalize to new data, as it is likely overfit to our data.


```r
#4rd Tree combination
rpart_tree5 <- rpart(isSpam ~., data=spam_train, 
                     na.action = na.rpart, 
                     method = "class",
                     control=rpart.control(minsplit = 1, 
                                   maxdepth=30, 
                                   minbucket = 1, 
                                   cp = 1e-7))

rpart.plot(rpart_tree5)
```

<div class="figure" style="text-align: center">
<img src="Case_study_6_files/figure-html/unnamed-chunk-8-1.svg" alt="**Figure 4 :** A massive tree!"  />
<p class="caption">**Figure 4 :** A massive tree!</p>
</div>

This is a massive, likely overfit  tree, typical of what you would see in a random forest. Lets see how much better it did than the previous iterations.


```r
rp_predict5 <- predict(rpart_tree5,
                      newdata = spam_test,
                      type = "class")

result_5 <- make_metric_frame(rpart_tree5, rp_predict5)
result_5
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["leaves"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["depth"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["accuracy"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["precision.F"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["precision.T"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["recall.F"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["recall.T"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["f1_score"],"name":[8],"type":["dbl"],"align":["right"]}],"data":[{"1":"286","2":"28","3":"0.95","4":"0.97","5":"0.89","6":"0.96","7":"0.91","8":"0.9"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

This tree clearly outperformed the rest, with very high scores across the board. However, I would be wary of using it in production on new data, given the naturally high bias of large decision trees. It would be best to combine a large ensemble of these in a random forest.

# Conclusion

Lets first look at all of the model metrics on top of each other, so we can easily compare models in a single paragraph:


```r
rbind("default" = result_0, "small" = result_2, "large"=result_3, "giant"=result_5)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["leaves"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["depth"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["accuracy"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["precision.F"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["precision.T"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["recall.F"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["recall.T"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["f1_score"],"name":[8],"type":["dbl"],"align":["right"]}],"data":[{"1":"17","2":"10","3":"0.90","4":"0.93","5":"0.83","6":"0.94","7":"0.80","8":"0.81","_rn_":"default"},{"1":"3","2":"2","3":"0.83","4":"0.97","5":"0.43","6":"0.83","7":"0.84","8":"0.57","_rn_":"small"},{"1":"25","2":"11","3":"0.91","4":"0.93","5":"0.87","6":"0.95","7":"0.80","8":"0.83","_rn_":"large"},{"1":"286","2":"28","3":"0.95","4":"0.97","5":"0.89","6":"0.96","7":"0.91","8":"0.90","_rn_":"giant"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

The area most strongly affected by tree size it seems is the precision of the minority class. This intuitively makes sense, as a small tree does not look at the data at a very granular level; it will not detect the subtle patterns of the minority class. A larger tree however will be easily able to see and find rules which allow it to properly fit the minority class. The results of this study overall are not too interesting theoretically, it is well known that small trees are "weak predictors" and large trees are strong (often overfit) predictors.


## References
[1] D. Lang and D. Nolan, Data Science in R: A Case Studies Approach to Computation Reasoning and Problem Solving. New York, New York: CRC Press. 


# Appendix: Data Cleaning Code

The code for cleaning and loading the data is shown below. It does not appear to work on linux, and thus must be run on windows. Note it is mostly unmodified from the starter code from Dr. Slater. The only change of note is the data was saved to a csv instead of an RDA file.



```r
#spamPath = system.file(package = "RSpamData")
#spamPath = "/Users/nolan/RPackages/RSpamData"spamPath = "."
spamPath = "./spam"
#list.dirs(spamPath, full.names = FALSE)



list.files(path = paste(spamPath, "messages", 
                        sep = .Platform$file.sep))



f=list.files(path = paste(spamPath,'messages/spam', sep=.Platform$file.sep))

length(f)

head(list.files(path = paste(spamPath, "messages", "spam_2",
                             sep = .Platform$file.sep)))

dirNames = list.files(path = paste(spamPath, "messages", 
                      sep = .Platform$file.sep))
length(list.files(paste(spamPath, "messages", dirNames, 
                        sep = .Platform$file.sep)))

sapply(paste(spamPath, "messages", dirNames, 
             sep = .Platform$file.sep), 
       function(dir) length(list.files(dir)) )

fullDirNames = paste(spamPath, "messages", dirNames, 
                     sep = .Platform$file.sep)

fileNames = list.files(fullDirNames[1], full.names = TRUE)
fileNames[1]

msg = readLines(fileNames[1])
head(msg)

indx = c(1:5, 15, 27, 68, 69, 329, 404, 427, 516, 852, 971)
fn = list.files(fullDirNames[1], full.names = TRUE)[indx]
sampleEmail = sapply(fn, readLines)        

msg = sampleEmail[[1]]
which(msg == "")[1]

match("", msg)

splitPoint = match("", msg)

msg[ (splitPoint - 2):(splitPoint + 6) ]

header = msg[1:(splitPoint-1)]
body = msg[ -(1:splitPoint) ]

splitMessage = function(msg) {
  splitPoint = match("", msg)
  header = msg[1:(splitPoint-1)]
  body = msg[ -(1:splitPoint) ]
  return(list(header = header, body = body))
}

sampleSplit = lapply(sampleEmail, splitMessage)

header = sampleSplit[[1]]$header
grep("Content-Type", header)

grep("multi", tolower(header[46]))

header[46]

headerList = lapply(sampleSplit, function(msg) msg$header)
CTloc = sapply(headerList, grep, pattern = "Content-Type")
CTloc

sapply(headerList, function(header) {
                       CTloc = grep("Content-Type", header)
                       if (length(CTloc) == 0) return(NA)
                       CTloc
                   })

hasAttach = sapply(headerList, function(header) {
  CTloc = grep("Content-Type", header)
  if (length(CTloc) == 0) return(FALSE)
  grepl("multi", tolower(header[CTloc])) 
})

hasAttach

header = sampleSplit[[6]]$header
boundaryIdx = grep("boundary=", header)
header[boundaryIdx]

sub(".*boundary=\"(.*)\";.*", "\\1", header[boundaryIdx])

header2 = headerList[[9]]
boundaryIdx2 = grep("boundary=", header2)
header2[boundaryIdx2]

sub('.*boundary="(.*)";.*', "\\1", header2[boundaryIdx2])

boundary2 = gsub('"', "", header2[boundaryIdx2])

sub(".*boundary= *(.*);?.*", "\\1", boundary2)

boundary = gsub('"', "", header[boundaryIdx])
sub(".*boundary= *(.*);?.*", "\\1", boundary)

sub(".*boundary= *([^;]*);?.*", "\\1", boundary)

getBoundary = function(header) {
  boundaryIdx = grep("boundary=", header)
  boundary = gsub('"', "", header[boundaryIdx])
  gsub(".*boundary= *([^;]*);?.*", "\\1", boundary)
}

sampleSplit[[6]]$body

boundary = getBoundary(headerList[[15]]) 
body = sampleSplit[[15]]$body

bString = paste("--", boundary, sep = "")
bStringLocs = which(bString == body)
bStringLocs

eString = paste("--", boundary, "--", sep = "")
eStringLoc = which(eString == body)
eStringLoc

msg = body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1)]
tail(msg)

msg = c(msg, body[ (eStringLoc + 1) : length(body) ])
tail(msg)

dropAttach = function(body, boundary){
  
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  
  if (length(bStringLocs) <= 1) return(body)
  
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  if (length(eStringLoc) == 0) 
    return(body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1)])
  
  n = length(body)
  if (eStringLoc < n) 
     return( body[ c( (bStringLocs[1] + 1) : (bStringLocs[2] - 1), 
                    ( (eStringLoc + 1) : n )) ] )
  
  return( body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1) ])
}

head(sampleSplit[[1]]$body)

msg = sampleSplit[[3]]$body
head(msg)

msg[ c(1, 3, 26, 27) ]

cleanMsg = tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", msg))
cleanMsg[ c(1, 3, 26, 27) ]

library(tm)
stopWords = stopwords()
cleanSW = tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", stopWords))
SWords = unlist(strsplit(cleanSW, "[[:blank:]]+"))
SWords = SWords[ nchar(SWords) > 1 ]
stopWords = unique(SWords)

words = unlist(strsplit(cleanMsg, "[[:blank:]]+"))

words = words[ nchar(words) > 1 ]

words = words[ !( words %in% stopWords) ]
head(words)

cleanText =
function(msg)   {
  tolower(gsub("[[:punct:]0-9[:space:][:blank:]]+", " ", msg))
}

findMsgWords = 
function(msg, stopWords) {
 if(is.null(msg))
  return(character())

 words = unique(unlist(strsplit(cleanText(msg), "[[:blank:]\t]+")))
 
 # drop empty and 1 letter words
 words = words[ nchar(words) > 1]
 words = words[ !( words %in% stopWords) ]
 invisible(words)
}

processAllWords = function(dirName, stopWords)
{
       # read all files in the directory
  fileNames = list.files(dirName, full.names = TRUE)
       # drop files that are not email, i.e., cmds
  notEmail = grep("cmds$", fileNames)
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]

  messages = lapply(fileNames, readLines, encoding = "utf-8")
  
       # split header and body
  emailSplit = lapply(messages, splitMessage)
       # put body and header in own lists
  bodyList = lapply(emailSplit, function(msg) msg$body)
  headerList = lapply(emailSplit, function(msg) msg$header)
  rm(emailSplit)
  
       # determine which messages have attachments
  hasAttach = sapply(headerList, function(header) {
    CTloc = grep("Content-Type", header)
    if (length(CTloc) == 0) return(0)
    multi = grep("multi", tolower(header[CTloc])) 
    if (length(multi) == 0) return(0)
    multi
  })
  
  hasAttach = which(hasAttach > 0)
  
       # find boundary strings for messages with attachments
  boundaries = sapply(headerList[hasAttach], getBoundary)
  
       # drop attachments from message body
  bodyList[hasAttach] = mapply(dropAttach, bodyList[hasAttach], 
                               boundaries, SIMPLIFY = FALSE)
  
       # extract words from body
  msgWordsList = lapply(bodyList, findMsgWords, stopWords)
  
  invisible(msgWordsList)
}



msgWordsList = lapply(fullDirNames, processAllWords, 
                      stopWords = stopWords) 



numMsgs = sapply(msgWordsList, length)
numMsgs

isSpam = rep(c(FALSE, FALSE, FALSE, TRUE, TRUE), numMsgs)

msgWordsList = unlist(msgWordsList, recursive = FALSE)

numEmail = length(isSpam)
numSpam = sum(isSpam)
numHam = numEmail - numSpam

set.seed(418910)

testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))

testMsgWords = c((msgWordsList[isSpam])[testSpamIdx],
                 (msgWordsList[!isSpam])[testHamIdx] )
trainMsgWords = c((msgWordsList[isSpam])[ - testSpamIdx], 
                  (msgWordsList[!isSpam])[ - testHamIdx])

testIsSpam = rep(c(TRUE, FALSE), 
                 c(length(testSpamIdx), length(testHamIdx)))
trainIsSpam = rep(c(TRUE, FALSE), 
                 c(numSpam - length(testSpamIdx), 
                   numHam - length(testHamIdx)))

bow = unique(unlist(trainMsgWords))

length(bow)

spamWordCounts = rep(0, length(bow))

names(spamWordCounts) = bow

tmp = lapply(trainMsgWords[trainIsSpam], unique)
tt = table( unlist(tmp) )
spamWordCounts[ names(tt) ] = tt

computeFreqs =
function(wordsList, spam, bow = unique(unlist(wordsList)))
{
   # create a matrix for spam, ham, and log odds
  wordTable = matrix(0.5, nrow = 4, ncol = length(bow), 
                     dimnames = list(c("spam", "ham", 
                                        "presentLogOdds", 
                                        "absentLogOdds"),  bow))

   # For each spam message, add 1 to counts for words in message
  counts.spam = table(unlist(lapply(wordsList[spam], unique)))
  wordTable["spam", names(counts.spam)] = counts.spam + .5

   # Similarly for ham messages
  counts.ham = table(unlist(lapply(wordsList[!spam], unique)))  
  wordTable["ham", names(counts.ham)] = counts.ham + .5  


   # Find the total number of spam and ham
  numSpam = sum(spam)
  numHam = length(spam) - numSpam

   # Prob(word|spam) and Prob(word | ham)
  wordTable["spam", ] = wordTable["spam", ]/(numSpam + .5)
  wordTable["ham", ] = wordTable["ham", ]/(numHam + .5)
  
   # log odds
  wordTable["presentLogOdds", ] = 
     log(wordTable["spam",]) - log(wordTable["ham", ])
  wordTable["absentLogOdds", ] = 
     log((1 - wordTable["spam", ])) - log((1 -wordTable["ham", ]))

  invisible(wordTable)
}

trainTable = computeFreqs(trainMsgWords, trainIsSpam)

newMsg = testMsgWords[[1]]

newMsg = newMsg[!is.na(match(newMsg, colnames(trainTable)))]

present = colnames(trainTable) %in% newMsg

sum(trainTable["presentLogOdds", present]) + 
  sum(trainTable["absentLogOdds", !present])

newMsg = testMsgWords[[ which(!testIsSpam)[1] ]]
newMsg = newMsg[!is.na(match(newMsg, colnames(trainTable)))]
present = (colnames(trainTable) %in% newMsg)
sum(trainTable["presentLogOdds", present]) + 
     sum(trainTable["absentLogOdds", !present])

computeMsgLLR = function(words, freqTable) 
{
       # Discards words not in training data.
  words = words[!is.na(match(words, colnames(freqTable)))]

       # Find which words are present
  present = colnames(freqTable) %in% words

  sum(freqTable["presentLogOdds", present]) +
    sum(freqTable["absentLogOdds", !present])
}

testLLR = sapply(testMsgWords, computeMsgLLR, trainTable)

tapply(testLLR, testIsSpam, summary)


#pdf("SP_Boxplot.pdf", width = 6, height = 6)
spamLab = c("ham", "spam")[1 + testIsSpam]
boxplot(testLLR ~ spamLab, ylab = "Log Likelihood Ratio",
      #  main = "Log Likelihood Ratio for Randomly Chosen Test Messages",
        ylim=c(-500, 500))
#dev.off()


typeIErrorRate = 
function(tau, llrVals, spam)
{
  classify = llrVals > tau
  sum(classify & !spam)/sum(!spam)
}

typeIErrorRate(0, testLLR,testIsSpam)

typeIErrorRate(-20, testLLR,testIsSpam)

typeIErrorRates = 
function(llrVals, isSpam) 
{
  o = order(llrVals)
  llrVals =  llrVals[o]
  isSpam = isSpam[o]

  idx = which(!isSpam)
  N = length(idx)
  list(error = (N:1)/N, values = llrVals[idx])
}

typeIIErrorRates = function(llrVals, isSpam) {
    
  o = order(llrVals)
  llrVals =  llrVals[o]
  isSpam = isSpam[o]
    
    
  idx = which(isSpam)
  N = length(idx)
  list(error = (1:(N))/N, values = llrVals[idx])
  }  

xI = typeIErrorRates(testLLR, testIsSpam)
xII = typeIIErrorRates(testLLR, testIsSpam)
tau01 = round(min(xI$values[xI$error <= 0.01]))
t2 = max(xII$error[ xII$values < tau01 ])

#pdf("LinePlotTypeI+IIErrors.pdf", width = 8, height = 6)

library(RColorBrewer)
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(xII$error ~ xII$values,  type = "l", col = cols[1], lwd = 3,
     xlim = c(-300, 250), ylim = c(0, 1),
     xlab = "Log Likelihood Ratio Values", ylab="Error Rate")
points(xI$error ~ xI$values, type = "l", col = cols[2], lwd = 3)
legend(x = 50, y = 0.4, fill = c(cols[2], cols[1]),
       legend = c("Classify Ham as Spam", 
                  "Classify Spam as Ham"), cex = 0.8,
       bty = "n")
abline(h=0.01, col ="grey", lwd = 3, lty = 2)
text(-250, 0.05, pos = 4, "Type I Error = 0.01", col = cols[2])

mtext(tau01, side = 1, line = 0.5, at = tau01, col = cols[3])
segments(x0 = tau01, y0 = -.50, x1 = tau01, y1 = t2, 
         lwd = 2, col = "grey")
text(tau01 + 20, 0.05, pos = 4,
     paste("Type II Error = ", round(t2, digits = 2)), 
     col = cols[1])

#dev.off()

k = 5
numTrain = length(trainMsgWords)
partK = sample(numTrain)
tot = k * floor(numTrain/k)
partK = matrix(partK[1:tot], ncol = k)

testFoldOdds = NULL
for (i in 1:k) {
  foldIdx = partK[ , i]
  trainTabFold = computeFreqs(trainMsgWords[-foldIdx], trainIsSpam[-foldIdx])
  testFoldOdds = c(testFoldOdds, 
               sapply(trainMsgWords[ foldIdx ], computeMsgLLR, trainTabFold))
}

testFoldSpam = NULL
for (i in 1:k) {
  foldIdx = partK[ , i]
  testFoldSpam = c(testFoldSpam, trainIsSpam[foldIdx])
}

xFoldI = typeIErrorRates(testFoldOdds, testFoldSpam)
xFoldII = typeIIErrorRates(testFoldOdds, testFoldSpam)
tauFoldI = round(min(xFoldI$values[xFoldI$error <= 0.01]))
tFold2 = xFoldII$error[ xFoldII$values < tauFoldI ]

smallNums = rep((1/2)^40, 2000000)

largeNum = 10000

print(sum(smallNums), digits = 20)

print(largeNum + sum(smallNums), digits = 20)

for (i in 1:length(smallNums)) {
  largeNum = largeNum + smallNums[i]
}
print(largeNum, digits = 20)

sampleSplit = lapply(sampleEmail, splitMessage)

header = sampleSplit[[1]]$header
header[1:12]

header[1] = sub("^From", "Top-From:", header[1])
header[1]

headerPieces = read.dcf(textConnection(header), all = TRUE)

headerPieces[, "Delivered-To"]


headerVec = unlist(headerPieces)
dupKeys = sapply(headerPieces, function(x) length(unlist(x)))
names(headerVec) = rep(colnames(headerPieces), dupKeys)

headerVec[ which(names(headerVec) == "Delivered-To") ]

length(headerVec)

length(unique(names(headerVec)))

processHeader = function(header)
{
       # modify the first line to create a key:value pair
  header[1] = sub("^From", "Top-From:", header[1])
  
  headerMat = read.dcf(textConnection(header), all = TRUE)
  headerVec = unlist(headerMat)
  
  dupKeys = sapply(headerMat, function(x) length(unlist(x)))
  names(headerVec) = rep(colnames(headerMat), dupKeys)
  
  return(headerVec)
}


headerList = lapply(sampleSplit, 
                    function(msg) {
                      processHeader(msg$header)} )

contentTypes = sapply(headerList, function(header) 
                                        header["Content-Type"])
names(contentTypes) = NULL
contentTypes

hasAttach = grep("^ *multi", tolower(contentTypes))
hasAttach

boundaries = getBoundary(contentTypes[ hasAttach ])
boundaries

boundary = boundaries[9]
body = sampleSplit[[15]]$body

bString = paste("--", boundary, sep = "")
bStringLocs = which(bString == body)
bStringLocs

eString = paste("--", boundary, "--", sep = "")
eStringLoc = which(eString == body)
eStringLoc

diff(c(bStringLocs[-1], eStringLoc))

### This code has mistakes in it - and we fix them later!
processAttach = function(body, contentType){
  
  boundary = getBoundary(contentType)
  
  bString = paste("--", boundary, "$", sep = "")
  bStringLocs = grep(bString, body)
  
  eString = paste("--", boundary, "--$", sep = "")
  eStringLoc = grep(eString, body)
  
  n = length(body)
  
  if (length(eStringLoc) == 0) eStringLoc = n + 1
  if (length(bStringLocs) == 1) attachLocs = NULL
  else attachLocs = c(bStringLocs[-1],  eStringLoc)
  
  msg = body[ (bStringLocs[1] + 1) : min(n, (bStringLocs[2] - 1), 
                                         na.rm = TRUE)]
  
  if ( eStringLoc < n )
    msg = c(msg, body[ (eStringLoc + 1) : n ])
  
  if ( !is.null(attachLocs) ) {
    attachLens = diff(attachLocs, lag = 1) 
    attachTypes = mapply(function(begL, endL) {
      contentTypeLoc = grep("[Cc]ontent-[Tt]ype", body[ (begL + 1) : (endL - 1)])
      contentType = body[ begL + contentTypeLoc]
      contentType = gsub('"', "", contentType )
      MIMEType = sub(" *Content-Type: *([^;]*);?.*", "\\1", contentType)
      return(MIMEType)
    }, attachLocs[-length(attachLocs)], attachLocs[-1])
  }
  
  if (is.null(attachLocs)) return(list(body = msg, attachInfo = NULL) )
  else return(list(body = msg, 
                   attachDF = data.frame(aLen = attachLens, 
                                         aType = attachTypes,
                                         stringsAsFactors = FALSE)))                                
}


bodyList = lapply(sampleSplit, function(msg) msg$body)
attList = mapply(processAttach, bodyList[hasAttach], 
                 contentTypes[hasAttach], 
                 SIMPLIFY = FALSE)

lens = sapply(attList, function(processedA) 
                           processedA$attachDF$aLen)
head(lens)  # will cause an error with the first message. its ok.
attList[[2]]$attachDF

body = bodyList[hasAttach][[2]]
length(body)

body[35:45]

processAttach = function(body, contentType){

  n = length(body)
  boundary = getBoundary(contentType)
 
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  
  if (length(eStringLoc) == 0) eStringLoc = n
  if (length(bStringLocs) <= 1) {
    attachLocs = NULL
    msgLastLine = n
    if (length(bStringLocs) == 0) bStringLocs = 0
  } else {
    attachLocs = c(bStringLocs[ -1 ],  eStringLoc)
    msgLastLine = bStringLocs[2] - 1
  }
  
  msg = body[ (bStringLocs[1] + 1) : msgLastLine] 
  if ( eStringLoc < n )
    msg = c(msg, body[ (eStringLoc + 1) : n ])
  
  if ( !is.null(attachLocs) ) {
    attachLens = diff(attachLocs, lag = 1) 
    attachTypes = mapply(function(begL, endL) {
      CTloc = grep("^[Cc]ontent-[Tt]ype", body[ (begL + 1) : (endL - 1)])
      if ( length(CTloc) == 0 ) {
        MIMEType = NA
      } else {
        CTval = body[ begL + CTloc[1] ]
        CTval = gsub('"', "", CTval )
        MIMEType = sub(" *[Cc]ontent-[Tt]ype: *([^;]*);?.*", "\\1", CTval)   
      }
      return(MIMEType)
    }, attachLocs[-length(attachLocs)], attachLocs[-1])
  }
  
  if (is.null(attachLocs)) return(list(body = msg, attachDF = NULL) )
  return(list(body = msg, 
             attachDF = data.frame(aLen = attachLens, 
                                     aType = unlist(attachTypes),
                                     stringsAsFactors = FALSE)))                                
}                       

readEmail = function(dirName) {
       # retrieve the names of files in directory
  fileNames = list.files(dirName, full.names = TRUE)
       # drop files that are not email
  notEmail = grep("cmds$", fileNames)
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]

       # read all files in the directory
  lapply(fileNames, readLines, encoding = "latin1")
}



processAllEmail = function(dirName, isSpam = FALSE)
{
       # read all files in the directory
  messages = readEmail(dirName)
  fileNames = names(messages)
  n = length(messages)
  
       # split header from body
  eSplit = lapply(messages, splitMessage)
  rm(messages)

       # process header as named character vector
  headerList = lapply(eSplit, function(msg) 
                                 processHeader(msg$header))
  
       # extract content-type key
  contentTypes = sapply(headerList, function(header) 
                                       header["Content-Type"])
  
       # extract the body
  bodyList = lapply(eSplit, function(msg) msg$body)
  rm(eSplit)

       # which email have attachments
  hasAttach = grep("^ *multi", tolower(contentTypes))

       # get summary stats for attachments and the shorter body
  attList = mapply(processAttach, bodyList[hasAttach], 
                   contentTypes[hasAttach], SIMPLIFY = FALSE)
  
  bodyList[hasAttach] = lapply(attList, function(attEl) 
                                           attEl$body)
 
  attachInfo = vector("list", length = n )
  attachInfo[ hasAttach ] = lapply(attList, 
                                  function(attEl) attEl$attachDF)
 
       # prepare return structure
  emailList = mapply(function(header, body, attach, isSpam) {
                       list(isSpam = isSpam, header = header, 
                            body = body, attach = attach)
                     },
                     headerList, bodyList, attachInfo, 
                     rep(isSpam, n), SIMPLIFY = FALSE )
  names(emailList) = fileNames
  
  invisible(emailList)
}

emailStruct = mapply(processAllEmail, fullDirNames,
                     isSpam = rep( c(FALSE, TRUE), 3:2))      
emailStruct = unlist(emailStruct, recursive = FALSE)

sampleStruct = emailStruct[ indx ]

save(emailStruct, file="emailXX.rda")

header = sampleStruct[[1]]$header
subject = header["Subject"]
els = strsplit(subject, "")
all(els %in% LETTERS)

testSubject = c("DEAR MADAME", "WINNER!", "")

els = strsplit(testSubject, "")
sapply(els, function(subject) all(subject %in% LETTERS))


gsub("[[:punct:] ]", "", testSubject)

gsub("[^[:alpha:]]", "", testSubject)


isYelling = function(msg) {
  if ( "Subject" %in% names(msg$header) ) {
     el = gsub("[^[:alpha:]]", "", msg$header["Subject"])
     if (nchar(el) > 0) 
        nchar(gsub("[A-Z]", "", el)) < 1
     else 
        FALSE
  } else 
    NA
}

perCaps =
function(msg)
{
  body = paste(msg$body, collapse = "")

       # Return NA if the body of the message is "empty"
  if(length(body) == 0 || nchar(body) == 0) return(NA)

       # Eliminate non-alpha characters
  body = gsub("[^[:alpha:]]", "", body)
  capText = gsub("[^A-Z]", "", body)
  100 * nchar(capText)/nchar(body)
}


sapply(sampleStruct, perCaps)

funcList = list( 
 isRe = function(msg) {
       "Subject" %in% names(msg$header) &&
         length(grep("^[ \t]*Re:", msg$header[["Subject"]])) > 0
  },
  numLines = function(msg) 
                length(msg$body),
  isYelling = function(msg) {
    if ( "Subject" %in% names(msg$header) ) {
       el = gsub("[^[:alpha:]]", "", msg$header["Subject"])
       if (nchar(el) > 0) 
         nchar(gsub("[A-Z]", "", el)) < 1
       else 
         FALSE
    }
    else NA
  },
  perCaps = function(msg) {
    body = paste(msg$body, collapse = "")

         # Return NA if the body of the message is "empty"
    if(length(body) == 0 || nchar(body) == 0) return(NA)

         # Eliminate non-alpha characters
    body = gsub("[^[:alpha:]]", "", body)
    capText = gsub("[^A-Z]", "", body)
    100 * nchar(capText)/nchar(body)
  }
)

lapply(funcList, function(func) 
                   sapply(sampleStruct, function(msg) func(msg)))

createDerivedDF =
function(email = emailStruct, operations = funcList, 
         verbose = FALSE)
{
  els = lapply(names(operations),
               function(id) {
                 if(verbose) print(id)
                 e = operations[[id]]
                 v = if(is.function(e)) 
                        sapply(email, e)
                      else 
                        sapply(email, function(msg) eval(e))
                 v
         })

   df = as.data.frame(els)
   names(df) = names(operations)
   invisible(df)
}

sampleDF = createDerivedDF(sampleStruct)
head(sampleDF)

funcList = list(
  isSpam =
    expression(msg$isSpam)
  ,
  isRe =
    function(msg) {
      # Can have a Fwd: Re:  ... but we are not looking for this here.
      # We may want to look at In-Reply-To field.
      "Subject" %in% names(msg$header) && 
        length(grep("^[ \t]*Re:", msg$header[["Subject"]])) > 0
    }
  ,
  numLines =
    function(msg) length(msg$body)
  ,
  bodyCharCt =
    function(msg)
      sum(nchar(msg$body))
  ,
  underscore =
    function(msg) {
      if(!"Reply-To" %in% names(msg$header))
        return(FALSE)
      
      txt <- msg$header[["Reply-To"]]
      length(grep("_", txt)) > 0  && 
        length(grep("[0-9A-Za-z]+", txt)) > 0
    }
  ,
  subExcCt = 
    function(msg) {
      x = msg$header["Subject"]
      if(length(x) == 0 || sum(nchar(x)) == 0 || is.na(x))
        return(NA)
      
      sum(nchar(gsub("[^!]","", x)))
    }
  ,
  subQuesCt =
    function(msg) {
      x = msg$header["Subject"]
      if(length(x) == 0 || sum(nchar(x)) == 0 || is.na(x))
        return(NA)
      
      sum(nchar(gsub("[^?]","", x)))
    }
  ,
  numAtt = 
    function(msg) {
      if (is.null(msg$attach)) return(0)
      else nrow(msg$attach)
    }
   
  ,
  priority =
    function(msg) {
      ans <- FALSE
      # Look for names X-Priority, Priority, X-Msmail-Priority
      # Look for high any where in the value
      ind = grep("priority", tolower(names(msg$header)))
      if (length(ind) > 0)  {
        ans <- length(grep("high", tolower(msg$header[ind]))) >0
      }
      ans
    }
  ,
  numRec =
    function(msg) {
      # unique or not.
      els = getMessageRecipients(msg$header)
      
      if(length(els) == 0)
        return(NA)
      
      # Split each line by ","  and in each of these elements, look for
      # the @ sign. This handles
      tmp = sapply(strsplit(els, ","), function(x) grep("@", x))
      sum(sapply(tmp, length))
    }
  ,
  perCaps =
    function(msg)
    {
      body = paste(msg$body, collapse = "")
      
      # Return NA if the body of the message is "empty"
      if(length(body) == 0 || nchar(body) == 0) return(NA)
      
      # Eliminate non-alpha characters and empty lines 
      body = gsub("[^[:alpha:]]", "", body)
      els = unlist(strsplit(body, ""))
      ctCap = sum(els %in% LETTERS)
      100 * ctCap / length(els)
    }
  ,
  isInReplyTo =
    function(msg)
    {
      "In-Reply-To" %in% names(msg$header)
    }
  ,
  sortedRec =
    function(msg)
    {
      ids = getMessageRecipients(msg$header)
      all(sort(ids) == ids)
    }
  ,
  subPunc =
    function(msg)
    {
      if("Subject" %in% names(msg$header)) {
        el = gsub("['/.:@-]", "", msg$header["Subject"])
        length(grep("[A-Za-z][[:punct:]]+[A-Za-z]", el)) > 0
      }
      else
        FALSE
    },
  hour =
    function(msg)
    {
      date = msg$header["Date"]
      if ( is.null(date) ) return(NA)
      # Need to handle that there may be only one digit in the hour
      locate = regexpr("[0-2]?[0-9]:[0-5][0-9]:[0-5][0-9]", date)
      
      if (locate < 0)
        locate = regexpr("[0-2]?[0-9]:[0-5][0-9]", date)
      if (locate < 0) return(NA)
      
      hour = substring(date, locate, locate+1)
      hour = as.numeric(gsub(":", "", hour))
      
      locate = regexpr("PM", date)
      if (locate > 0) hour = hour + 12
      
      locate = regexpr("[+-][0-2][0-9]00", date)
      if (locate < 0) offset = 0
      else offset = as.numeric(substring(date, locate, locate + 2))
      (hour - offset) %% 24
    }
  ,
  multipartText =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      numAtt = nrow(msg$attach)
      
      types = 
        length(grep("(html|plain|text)", msg$attach$aType)) > (numAtt/2)
    }
  ,
  hasImages =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      
      length(grep("^ *image", tolower(msg$attach$aType))) > 0
    }
  ,
  isPGPsigned =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      
      length(grep("pgp", tolower(msg$attach$aType))) > 0
    },
  perHTML =
    function(msg)
    {
      if(! ("Content-Type" %in% names(msg$header))) return(0)
      
      el = tolower(msg$header["Content-Type"]) 
      if (length(grep("html", el)) == 0) return(0)
      
      els = gsub("[[:space:]]", "", msg$body)
      totchar = sum(nchar(els))
      totplain = sum(nchar(gsub("<[^<]+>", "", els )))
      100 * (totchar - totplain)/totchar
    },
  subSpamWords =
    function(msg)
    {
      if("Subject" %in% names(msg$header))
        length(grep(paste(SpamCheckWords, collapse = "|"), 
                    tolower(msg$header["Subject"]))) > 0
      else
        NA
    }
  ,
  subBlanks =
    function(msg)
    {
      if("Subject" %in% names(msg$header)) {
        x = msg$header["Subject"]
        # should we count blank subject line as 0 or 1 or NA?
        if (nchar(x) == 1) return(0)
        else 100 *(1 - (nchar(gsub("[[:blank:]]", "", x))/nchar(x)))
      } else NA
    }
  ,
  noHost =
    function(msg)
    {
      # Or use partial matching.
      idx = pmatch("Message-", names(msg$header))
      
      if(is.na(idx)) return(NA)
      
      tmp = msg$header[idx]
      return(length(grep(".*@[^[:space:]]+", tmp)) ==  0)
    }
  ,
  numEnd =
    function(msg)
    {
      # If we just do a grep("[0-9]@",  )
      # we get matches on messages that have a From something like
      # " \"marty66@aol.com\" <synjan@ecis.com>"
      # and the marty66 is the "user's name" not the login
      # So we can be more precise if we want.
      x = names(msg$header)
      if ( !( "From" %in% x) ) return(NA)
      login = gsub("^.*<", "", msg$header["From"])
      if ( is.null(login) ) 
        login = gsub("^.*<", "", msg$header["X-From"])
      if ( is.null(login) ) return(NA)
      login = strsplit(login, "@")[[1]][1]
      length(grep("[0-9]+$", login)) > 0
    },
  isYelling =
    function(msg)
    {
      if ( "Subject" %in% names(msg$header) ) {
        el = gsub("[^[:alpha:]]", "", msg$header["Subject"])
        if (nchar(el) > 0) nchar(gsub("[A-Z]", "", el)) < 1
        else FALSE
      }
      else
        NA
    },
  forwards =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0 || sum(nchar(x)) == 0)
        return(NA)
      
      ans = length(grep("^[[:space:]]*>", x))
      100 * ans / length(x)
    },
  isOrigMsg =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("^[^[:alpha:]]*original[^[:alpha:]]+message[^[:alpha:]]*$", 
                  tolower(x) ) ) > 0
    },
  isDear =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("^[[:blank:]]*dear +(sir|madam)\\>", 
                  tolower(x))) > 0
    },
  isWrote =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("(wrote|schrieb|ecrit|escribe):", tolower(x) )) > 0
    },
  avgWordLen =
    function(msg)
    {
      txt = paste(msg$body, collapse = " ")
      if(length(txt) == 0 || sum(nchar(txt)) == 0) return(0)
      
      txt = gsub("[^[:alpha:]]", " ", txt)
      words = unlist(strsplit(txt, "[[:blank:]]+"))
      wordLens = nchar(words)
      mean(wordLens[ wordLens > 0 ])
    }
  ,
  numDlr =
    function(msg)
    {
      x = paste(msg$body, collapse = "")
      if(length(x) == 0 || sum(nchar(x)) == 0)
        return(NA)
      
      nchar(gsub("[^$]","", x))
    }
)



SpamCheckWords =
  c("viagra", "pounds", "free", "weight", "guarantee", "million", 
    "dollars", "credit", "risk", "prescription", "generic", "drug",
    "financial", "save", "dollar", "erotic", "million", "barrister",
    "beneficiary", "easy", 
    "money back", "money", "credit card")


getMessageRecipients =
  function(header)
  {
    c(if("To" %in% names(header))  header[["To"]] else character(0),
      if("Cc" %in% names(header))  header[["Cc"]] else character(0),
      if("Bcc" %in% names(header)) header[["Bcc"]] else character(0)
    )
  }

emailDF = createDerivedDF(emailStruct)
dim(emailDF)
save(emailDF, file = "spamAssassinDerivedDF.rda")
# here is the only important bit
write.csv(emailDF, "../Data/data.csv")

#load("Data/spamAssassinDerivedDF.rda")
dim(emailDF)

perCaps2 =
function(msg)
{
  body = paste(msg$body, collapse = "")

       # Return NA if the body of the message is "empty"
  if(length(body) == 0 || nchar(body) == 0) return(NA)

       # Eliminate non-alpha characters and empty lines 
  body = gsub("[^[:alpha:]]", "", body)
  els = unlist(strsplit(body, ""))
  ctCap = sum(els %in% LETTERS)
  100 * ctCap / length(els)
}


pC = sapply(emailStruct, perCaps)
pC2 = sapply(emailStruct, perCaps2)
identical(pC, pC2)

indNA = which(is.na(emailDF$subExcCt))

indNoSubject = which(sapply(emailStruct, 
                            function(msg) 
                              !("Subject" %in% names(msg$header))))

all(indNA == indNoSubject)

all(emailDF$bodyCharCt > emailDF$numLines)


x.at = c(1,10,100,1000,10000,100000)
y.at = c(1, 5, 10, 50, 100, 500, 5000)
nL = 1 + emailDF$numLines
nC = 1 + emailDF$bodyCharCt

#pdf("ScatterPlotNumLinesNumChars.pdf", width = 6, height = 4.5)
plot(nL ~ nC, log = "xy", pch=".", xlim=c(1,100000), axes = FALSE,
     xlab = "Number of Characters", ylab = "Number of Lines")
box() 
axis(1, at = x.at, labels = formatC(x.at, digits = 0, format="d"))
axis(2, at = y.at, labels = formatC(y.at, digits = 0, format="d")) 
abline(a=0, b=1, col="red", lwd = 2)
dev.off()

#pdf("SPAM_boxplotsPercentCaps.pdf", width = 5, height = 5)

percent = emailDF$perCaps
isSpamLabs = factor(emailDF$isSpam, labels = c("ham", "spam"))
boxplot(log(1 + percent) ~ isSpamLabs,
        ylab = "Percent Capitals (log)")

#dev.off()

logPerCapsSpam = log(1 + emailDF$perCaps[ emailDF$isSpam ])
logPerCapsHam = log(1 + emailDF$perCaps[ !emailDF$isSpam ])

qqplot(logPerCapsSpam, logPerCapsHam, 
       xlab = "Regular Email", ylab = "Spam Email", 
       main = "Percentage of Capital Letters (log scale)",
       pch = 19, cex = 0.3)

#pdf("SPAM_scatterplotPercentCapsTotChars.pdf", width = 8, height = 6)
 
colI = c("#4DAF4A80", "#984EA380")
logBodyCharCt = log(1 + emailDF$bodyCharCt)
logPerCaps = log(1 + emailDF$perCaps)
plot(logPerCaps ~ logBodyCharCt, xlab = "Total Characters (log)",
     ylab = "Percent Capitals (log)",
     col = colI[1 + emailDF$isSpam],
     xlim = c(2,12), pch = 19, cex = 0.5)

#dev.off()

table(emailDF$numAtt, isSpamLabs)
#pdf("SPAM_mosaicPlots.pdf", width = 8, height = 4)

oldPar = par(mfrow = c(1, 2), mar = c(1,1,1,1))

colM = c("#E41A1C80", "#377EB880")
isRe = factor(emailDF$isRe, labels = c("no Re:", "Re:"))
mosaicplot(table(isSpamLabs, isRe), main = "",
           xlab = "", ylab = "", color = colM)

fromNE = factor(emailDF$numEnd, labels = c("No #", "#"))
mosaicplot(table(isSpamLabs, fromNE), color = colM,
           main = "", xlab="", ylab = "")

par(oldPar)

#dev.off()

library(rpart)

setupRpart = function(data) {
  logicalVars = which(sapply(data, is.logical))
  facVars = lapply(data[ , logicalVars], 
                   function(x) {
                      x = as.factor(x)
                      levels(x) = c("F", "T")
                      x
                   })
  cbind(facVars, data[ , - logicalVars])
}

emailDFrp = setupRpart(emailDF)



set.seed(418910)
testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))

testDF = 
  rbind( emailDFrp[ emailDFrp$isSpam == "T", ][testSpamIdx, ],
         emailDFrp[emailDFrp$isSpam == "F", ][testHamIdx, ] )
trainDF =
  rbind( emailDFrp[emailDFrp$isSpam == "T", ][-testSpamIdx, ], 
         emailDFrp[emailDFrp$isSpam == "F", ][-testHamIdx, ])

rpartFit = rpart(isSpam ~ ., data = trainDF, method = "class")


library(rpart.plot)
library(RColorBrewer)
#prp(rpartFit, extra = 1)


#pdf("SPAM_rpartTree.pdf", width = 7, height = 7)

rpart.plot(rpartFit, extra = 1)
#dev.off()

predictions = predict(rpartFit, 
       newdata = testDF[, names(testDF) != "isSpam"],
       type = "class")
 
predsForHam = predictions[ testDF$isSpam == "F" ]
summary(predsForHam)

sum(predsForHam == "T") / length(predsForHam)

predsForSpam = predictions[ testDF$isSpam == "T" ]
sum(predsForSpam == "F") / length(predsForSpam)

complexityVals = c(seq(0.00001, 0.0001, length=19),
                   seq(0.0001, 0.001, length=19), 
                   seq(0.001, 0.005, length=9),
                   seq(0.005, 0.01, length=9))

fits = lapply(complexityVals, function(x) {
         rpartObj = rpart(isSpam ~ ., data = trainDF,
                          method="class", 
                          control = rpart.control(cp=x) )
           
         predict(rpartObj, 
                 newdata = testDF[ , names(testDF) != "isSpam"],
                 type = "class")
        })

spam = testDF$isSpam == "T"
numSpam = sum(spam)
numHam = sum(!spam)
errs = sapply(fits, function(preds) {
                      typeI = sum(preds[ !spam ] == "T") / numHam
                      typeII = sum(preds[ spam ] == "F") / numSpam
                      c(typeI = typeI, typeII = typeII)
                     })



#pdf("SPAM_rpartTypeIandII.pdf", width = 8, height = 7)
library(RColorBrewer)
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(errs[1,] ~ complexityVals, type="l", col=cols[2], 
     lwd = 2, ylim = c(0,0.2), xlim = c(0,0.01), 
     ylab="Error", xlab="complexity parameter values")
points(errs[2,] ~ complexityVals, type="l", col=cols[1], lwd = 2)

text(x =c(0.003, 0.0035), y = c(0.12, 0.05), 
     labels=c("Type II Error", "Type I Error"))

minI = which(errs[1,] == min(errs[1,]))[1]
abline(v = complexityVals[minI], col ="grey", lty =3, lwd=2)

text(0.0007, errs[1, minI]+0.01, 
     formatC(errs[1, minI], digits = 2))
text(0.0007, errs[2, minI]+0.01, 
     formatC(errs[2, minI], digits = 3))

#dev.off()

save(emailDFrp,file="data.Rda")

library(caret)

setupRnum = function(data) {
  logicalVars = which(sapply(data, is.logical))
  facVars = lapply(data[ , logicalVars], 
                   function(x) {
                      x = as.numeric(x)
                   })
  cbind(facVars, data[ , - logicalVars])
}

emailDFnum = setupRnum(emailDF)

emailDFnum[is.na(emailDFnum)]<-0


cv_folds <- createFolds(emailDFnum$isSpam, k=5, list=TRUE, returnTrain = TRUE)
lengths(cv_folds)

library(MLmetrics)
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  p <- Precision(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  r <- Recall(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  fp <-sum(data$pred==0 & data$obs==1)/length(data$pred)  
 
  fn <-sum(data$pred==1 & data$obs==0)/length(data$pred)
    c(F1 = f1_val,
    prec = p,
    rec = r,
    Type_I_err=fp,
    Type_II_err=fn
   )
}

library(naivebayes)
library(e1071)
nb_grid<-expand.grid(laplace=c(0,0.1,0.3,0.5,1), usekernel=c(T,F), adjust=c(T,F))
train_control<-trainControl(method="cv", number=3, savePredictions = 'final',summaryFunction = f1)
model_nb<-caret::train(as.factor(isSpam) ~ .,data=emailDFnum, trControl = train_control, method='naive_bayes',tuneGrid = nb_grid)
model_nb

#Did the boss fool us with the folds?  Nope.
table(model_nb$pred['Resample'])

val<-seq(from = 0, to=0.01, by=0.0005)
cart_grid<-expand.grid(cp=val)
train_control<-trainControl(method="cv", number =5, savePredictions = 'final',summaryFunction = f1)
model_rpart<-caret::train(as.factor(isSpam) ~ .,data=emailDFnum, trControl = train_control, method='rpart',tuneGrid = cart_grid)
model_rpart

set.seed(54)
is_Spam_ct <- table(emailDFrp$isSpam)["T"]
is_Ham_ct <- table(emailDFrp$isSpam)["F"]

testSpamidx = sample(is_Spam_ct, size = floor(is_Spam_ct/3))
testHamidx = sample(is_Ham_ct, size = floor(is_Ham_ct/3))

testDF = 
  rbind( emailDFrp[ emailDFrp$isSpam == "T", ][testSpamIdx, ],
         emailDFrp[emailDFrp$isSpam == "F", ][testHamIdx, ] )
trainDF =
  rbind( emailDFrp[emailDFrp$isSpam == "T", ][-testSpamIdx, ], 
         emailDFrp[emailDFrp$isSpam == "F", ][-testHamIdx, ])
testcontrolparam1 <- rpart(isSpam ~., data=trainDF, na.action = na.rpart, method = "class",
                           control=rpart.control(minsplit = 3, maxdepth=30, minbucket = 2, cp = 0.010)) 

rpart.plot(testcontrolparam1, extra = 1)

predictions = predict(testcontrolparam1, 
       newdata = testDF[, names(testDF) != "isSpam"],
       type = "class")
 
predsForHam = predictions[ testDF$isSpam == "F" ]
summary(predsForHam)
sum(predsForHam == "T") / length(predsForHam)
predsForSpam = predictions[ testDF$isSpam == "T" ]
sum(predsForSpam == "F") / length(predsForSpam)



set.seed(54)
testSpamidx = sample(is_Spam_ct, size = floor(is_Spam_ct/2))
testHamidx = sample(is_Ham_ct, size = floor(is_Ham_ct/2))
                    
                    
testDF = 
  rbind( emailDFrp[ emailDFrp$isSpam == "T", ][testSpamIdx, ],
         emailDFrp[emailDFrp$isSpam == "F", ][testHamIdx, ] )
trainDF =
  rbind( emailDFrp[emailDFrp$isSpam == "T", ][-testSpamIdx, ], 
         emailDFrp[emailDFrp$isSpam == "F", ][-testHamIdx, ])
testcontrolparam2 <- rpart(isSpam ~., data=trainDF, na.action = na.rpart, method = "class",
                           control=rpart.control(minsplit = 3, maxdepth= 2, minbucket = 2, cp = 0.010)) 

rpart.plot(testcontrolparam2, extra = 1)


predictions = predict(testcontrolparam2, 
       newdata = testDF[, names(testDF) != "isSpam"],
       type = "class")
 
predsForHam = predictions[ testDF$isSpam == "F" ]
summary(predsForHam)
sum(predsForHam == "T") / length(predsForHam)
predsForSpam = predictions[ testDF$isSpam == "T" ]
sum(predsForSpam == "F") / length(predsForSpam)

set.seed(54)
testSpamidx = sample(is_Spam_ct, size = floor(is_Spam_ct/2))
testHamidx = sample(is_Ham_ct, size = floor(is_Ham_ct/2))
                    
                    
testDF = 
  rbind( emailDFrp[ emailDFrp$isSpam == "T", ][testSpamIdx, ],
         emailDFrp[emailDFrp$isSpam == "F", ][testHamIdx, ] )
trainDF =
  rbind( emailDFrp[emailDFrp$isSpam == "T", ][-testSpamIdx, ], 
         emailDFrp[emailDFrp$isSpam == "F", ][-testHamIdx, ])
testcontrolparam3 <- rpart(isSpam ~., data=trainDF, na.action = na.rpart, method = "class",
                           control=rpart.control(minsplit = 3, maxdepth=30, minbucket = 2, cp = 0.005)) 

rpart.plot(testcontrolparam3, extra = 1)

predictions = predict(testcontrolparam3, 
       newdata = testDF[, names(testDF) != "isSpam"],
       type = "class")
 
predsForHam = predictions[ testDF$isSpam == "F" ]
summary(predsForHam)
sum(predsForHam == "T") / length(predsForHam)
predsForSpam = predictions[ testDF$isSpam == "T" ]
sum(predsForSpam == "F") / length(predsForSpam)

set.seed(54)
testSpamidx = sample(is_Spam_ct, size = floor(is_Spam_ct/2))
testHamidx = sample(is_Ham_ct, size = floor(is_Ham_ct/2))
                    
                    
testDF = 
  rbind( emailDFrp[ emailDFrp$isSpam == "T", ][testSpamIdx, ],
         emailDFrp[emailDFrp$isSpam == "F", ][testHamIdx, ] )
trainDF =
  rbind( emailDFrp[emailDFrp$isSpam == "T", ][-testSpamIdx, ], 
         emailDFrp[emailDFrp$isSpam == "F", ][-testHamIdx, ])
testcontrolparam4 <- rpart(isSpam ~., data=trainDF, na.action = na.rpart, method = "class",
                           control=rpart.control(minsplit = 3, maxdepth=2, minbucket = 2, cp = 0.005)) 

rpart.plot(testcontrolparam4, extra = 1)

predictions = predict(testcontrolparam4, 
       newdata = testDF[, names(testDF) != "isSpam"],
       type = "class")
 
predsForHam = predictions[ testDF$isSpam == "F" ]
summary(predsForHam)
sum(predsForHam == "T") / length(predsForHam)
predsForSpam = predictions[ testDF$isSpam == "T" ]
sum(predsForSpam == "F") / length(predsForSpam)
```
