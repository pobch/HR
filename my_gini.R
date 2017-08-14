# Create my own gini function to understand how CART split the tree. 
library(combinat)
library(dplyr)
library(readr)
library(rpart)
library(rpart.plot)

# ------- create functions: (choose max gini, not min gini)
gini_index = function(classes,splitvar = NULL) {
  if (is.null(splitvar)) {
    base_prob = table(classes)/length(classes)
    return(sum(base_prob**2))
  }
  if(length(table(splitvar)) < 2){
    return(0)
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Node_Gini <- sum(crossprob[,1]**2)
  Yes_Node_Gini <- sum(crossprob[,2]**2)
  # weight gini by base_prob:
  return(sum(base_prob * c(No_Node_Gini,Yes_Node_Gini)))
}

# for numeric independent var: 
all_gini_num = function(df, target_col_name, features_col_range) {
  num.gini = data.frame()
  for(i in features_col_range){
    if(is.numeric(df[[i]])){
      print(paste0('number of NA in ', names(df)[i], ' = ', sum(is.na(df[i]))))
      all.threshold = unique(df[[i]])
      score = sapply(all.threshold, function(x) { gini_index(df[[target_col_name]], df[[i]] < x ) })
      tmp = data.frame(feature.name = rep(names(df)[i], length(all.threshold)),
                       threshold.num = all.threshold,
                       gini = score,
                       stringsAsFactors = F)
      num.gini = rbind(num.gini, tmp)
    } else {
      stop(paste0(names(df)[i],' is not numeric or Date'))
    }
  }
  return(num.gini)
}

# for categorical independent var:
all_gini_cat = function(df, target_col_num, features_col_range){
  cat.gini = data.frame()
  for(i in features_col_range){
    if(is.character(df[[i]])){
      print(paste0('processing ', names(df)[i], ', NA = ', sum(is.na(df[i]))))
      values = unique(df[[i]])
      for(group.num in 1:(length(values)/2)){
        group1.num = group.num
        group2.num = length(values) - group1.num
        group1.prob = as.data.frame(combn(values, group1.num))
        for(group1.case in 1:ncol(group1.prob)){
          gini.case = gini_index(df[[target_col_num]], df[[i]] %in% group1.prob[[group1.case]])
          tmp = data.frame(feature.name = names(df)[i],
                           threshold.cat.g1 = paste(group1.prob[[group1.case]], collapse = ', '),
                           threshold.cat.g2 = paste(values[!(values %in% group1.prob[[group1.case]])], collapse = ', '),
                           gini = gini.case,
                           stringsAsFactors = F)
          cat.gini = rbind(cat.gini, tmp)
        }
      }
    } else {
      stop(paste0(names(df)[i],' is not character'))
    }
  }
  return(cat.gini)
}

#--------------------------

# import dataset
hr = read_csv('HR_comma_sep.csv')
# reorder columns
hr = hr[, c(names(hr)[names(hr) != 'left'], 'left')]

# find column number of numeric/date and categorical independent vars: 
col.num = c()
col.cat = c()
for(i in 1:ncol(hr)){
  if(is.numeric(hr[[i]]) ){
    col.num = append(col.num, i)
  } else if(is.character(hr[[i]]) ) {
    col.cat = append(col.cat, i)
  }
}
col.num = col.num[-length(col.num)] 

# check if number of columns is correct:
ncol(hr)
length(col.cat) + length(col.num)

# call gini function for numeric/date and categorical:
gini.num = all_gini_num(hr, 'left', col.num)
gini.cat = all_gini_cat(hr, 'left', col.cat)

# evaluate gini score:
conclude = gini.num %>% 
  bind_rows(gini.cat) %>% 
  group_by(feature.name) %>% 
  mutate(the.rank = rank(-gini, ties.method = 'random')) %>% 
  filter(the.rank == 1) %>% 
  select(-the.rank) %>% 
  arrange(desc(gini))
#-> max gini is 0.7413731, split by satisfaction_level > 0.47 (the same result as rpart)

model = rpart(left ~ ., data = hr, method = 'class')
prp(model)
