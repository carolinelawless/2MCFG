remove(list = ls())
setwd("C:/Users/Caroline/Documents/PhD/2MCFG")
set.seed(7)

source("functions.R")
source("likelihood_functions.R")

grammar <- "g0"
alpha1 <- 3 #scaling parameter for DP over nonterminals
alpha2 <- 3 #scaling parameter for DP over rules
gamma1 <- 2
alphabet<- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
#sentence <- sample(alphabet,50,replace=TRUE)
sentence<- alphabet[1:20]
p <- 0 #probability of emission (meaningless, just used in constrained function where we know proba of emission is 0)
proba_emission <- 0.4 #probability of emission
proba_epsilon<- 0.5 #probability of emission being empty

tt <- 1 #time

likelihoods<- vector()
rules<- list()
rules_left<- list()
rules_right<- list()
nonterminals_vec_all<- vector()
nonterminals_vec_all1<- 1
nonterminals_list_all<- list()
string1_list_all<- list()
string2_list_all<- list()
number_symbols_vec_all<- vector()

lklh<- 1

tree_matrix <- matrix(ncol = 8)
colnames(tree_matrix) <-c("ind_1", "ind_2", "n_children", "type", "min", "max", "B","nrows")
left_functions <- list()
right_functions <- list()

##'rows' and 'sides' represent the current path along the tree
rows <- 1
sides <- "left"
numbers<- 1
row <- tail(rows, 1)
side <- tail(sides, 1)
number<- tail(numbers,1)

tree_matrix[row, 2]<- 1
tree_matrix[row, 5]<- length(sentence)
tree_matrix[row, 6]<- length(sentence)
tree_matrix[row, 7]<- 1
tree_matrix[row,8]<- 1

stop<- FALSE

while(stop == FALSE){
  
  if(is.na(tree_matrix[row,4])){#then draw a new rule
    
    tree_matrix[row,8]<- nrow(tree_matrix)
    min<- tree_matrix[row, 5] #minimal number of terminal symbols associated with current nonterminal
    max<- tree_matrix[row, 6] #maximal number of terminal symbols associated with current nonterminal
    nonterminal<- tree_matrix[row, 7] #current nonterminal symbol
    rand<- sample(c(0, 1), 1, F, prob = c(proba_emission, 1 - proba_emission)) #emission or production
    
    if (min > 2 | rand == 1){#production rule
      nn <- max + 1
      while (nn > max) {
        rr <-dp_rule_random(gamma1,alpha1,nonterminals_vec_all1,terminals,grammar,p,alpha2,nonterminal,nonterminals_vec_all,nonterminals_list_all,string1_list_all,string2_list_all)
        nn <- length(rr[[3]])
      }
      rule <- list(nonterminal, rr[[3]], rr[[1]], rr[[2]])
      rule_left<- list(nonterminal,rr[[3]],rr[[1]])
      rule_right<- list(nonterminal, rr[[3]], rr[[2]])
      
      lklh<- lklh*likelihood_production(rule,alpha1,alpha2,gamma1,nonterminals_vec_all,proba_emission,rules)
      
      rules[[(length(rules)+1)]]<- rule
      rules_left[[(length(rules_left)+1)]]<- rule_left
      rules_right[[(length(rules_right)+1)]]<-rule_right
      nonterminals_vec_all[(length(nonterminals_vec_all)+1)]<- nonterminal
      nonterminals_vec_all1<- c(nonterminals_vec_all1,rule[[2]])
      nonterminals_list_all[[(length(nonterminals_list_all)+1)]]<- rule[[2]]
      string1_list_all[[(length(string1_list_all)+1)]]<- rule[[3]]
      string2_list_all[[(length(string2_list_all)+1)]]<- rule[[4]]
      
      left_functions[[row]] <- as.list(rr[[1]])
      right_functions[[row]] <- as.list(rr[[2]])
      tree_matrix[row, 3] <- length(rr[[3]])
      tree_matrix[row, 4] <- 1
      
      
      nc <- tree_matrix[row, 3] #number of children
      nr <- nrow(tree_matrix) #number of rows
      if (nc == 1) {
        tree_matrix <-rbind(tree_matrix, c(row, nr + 1, rep(NA, 2), min, max, rr[[3]][1],nr+nc))
        left_functions[[nr + 1]] <-list()
        right_functions[[nr + 1]] <-list()
      }else{
        for(i in 1:nc){
          tree_matrix <-rbind(tree_matrix, c(row, nr + i, rep(NA, 2), 1, max - nc + 1, rr[[3]][i],nr+nc))
          left_functions[[nr + i]] <- list()
          right_functions[[nr + i]] <- list()
        }
        tree_matrix[row,5]<- max(tree_matrix[row,5],nc)
        tree_matrix<- update_minmax1(rows,tree_matrix)
      }
    }else{#emission rule
      tree_matrix[row,3]<- 0
      tree_matrix[row,4]<- 2
      tree_matrix[row,5]<- min(tree_matrix[row,5],2)
      tree_matrix[row,6]<- min(tree_matrix[row,6],2)
      
      tree_matrix<- update_minmax1(rows,tree_matrix)
      
      draw<- sample(c(0,1),1,replace = FALSE,prob = c(proba_epsilon,1-proba_epsilon))
      if(draw==1 | tree_matrix[row,5]==2){
        x<- sentence[tt]
        }else if(draw==0){
          x<- ""
        }
      
      if (side == "left") {
        left_functions[[row]] <- x
        rule<- list(nonterminal,vector(),x,vector())
        rule_left<- list(nonterminal,vector(),x)
        rule_right<- list(nonterminal,vector(),vector())
      }else if(side=="right"){
        right_functions[[row]]<- x
        rule<- list(nonterminal,vector(),vector(),x)
        rule_left<- list(nonterminal,vector(),vector())
        rule_right<- list(nonterminal,vector(),x)
      }
      lklh<- lklh*likelihood_emission1(rule,rule_left,rule_right,side,alphabet,proba_emission,proba_epsilon,nonterminals_vec_all,rules,rules_left,rules_right)
      
      rules[[(length(rules)+1)]]<- rule
      rules_left[[(length(rules_left)+1)]]<- rule_left
      rules_right[[(length(rules_right)+1)]]<- rule_right
     
      if(x==sentence[tt]){
        likelihoods[tt]<- lklh
        tt<- tt+1
        lklh<- 1
      }
      
      if(length(rows)>1){
        row <- rows[(length(rows) - 1)]
        side <- sides[(length(sides) - 1)]
        numb<- number
        number<- numbers[(length(numbers) -1)]
        rows <- rows[1:(length(rows) - 1)]
        sides <- sides[1:(length(sides) - 1)]
        numbers<- numbers[1:(length(numbers) -1)]
        
        if (side == "left") {
          string1 <- left_functions[[row]]
          left_functions[[row]] <- evaluate2(string1,x,numb)
        } else if (side == "right") {
          string1 <- right_functions[[row]]
          right_functions[[row]] <- evaluate2(string1, x, numb)
        }
      }
    }
  }else if(tree_matrix[row,4]==1){
    
    if(side=="left"){
      string<- left_functions[[row]]
    }else if(side=="right"){
      string<- right_functions[[row]]
    }
    vec<- vector(length=length(string))
    for(k in 1:length(vec)){vec[k]<- is.numeric(string[[k]])}
    
    if(row==1 & side == "left" & sum(vec) == 0 ){
      sides<- "right"
      side<- "right"
    }else{
      
      if(sum(vec)==0){
        if(length(string)==1){
          if(is.na(string)==TRUE){
            x<- ""
          }else{x<- string}
        }else{
          x<- paste(string,collapse="")
        }
        if(side=="left"){
          left_functions[[row]]<- x
        }else if(side=="right"){
          right_functions[[row]]<- x
        }
        row <- rows[(length(rows) - 1)]
        side <- sides[(length(sides) - 1)]
        numb<- number
        number<- numbers[(length(numbers)-1)]
        rows <- rows[1:(length(rows) - 1)]
        sides <- sides[1:(length(sides) - 1)]
        numbers<- numbers[1:(length(numbers)-1)]
        if (side == "left") {
          string1 <- left_functions[[row]]
          left_functions[[row]] <- evaluate2(string1, x, numb)
        } else if (side == "right") {
          string1 <- right_functions[[row]]
          right_functions[[row]] <- evaluate2(string1, x, numb)
        }
        
      }else{
        
        tree_matrix<- update_minmax1(rows,tree_matrix)
        i<- 1
        while(is.numeric(string[[i]])==FALSE){i<- i+1}
        
        number <- string[[i]]
        if (ceiling(number / 2) == number / 2) {
          side <- "right"
        } else if (ceiling(number / 2) != number / 2) {
          side <- "left"
        }
        row <- tree_matrix[row,8] + ceiling(number / 2)
        sides <- c(sides, side)
        rows <- c(rows, row)
        numbers<- c(numbers, number)
        
        tree_matrix<- update_minmax2(rows,tree_matrix)
        
      }
    }
  }else if(tree_matrix[row,4]==2){
    tree_matrix<- update_minmax2(rows,tree_matrix)
    
    if(side=="left"){
      y<- right_functions[[row]]
    }else if(side=="right"){
      y<- left_functions[[row]]
    }
    
    draw<- sample(c(0,1),1,replace = FALSE,prob = c(proba_epsilon,1-proba_epsilon))
    if(tree_matrix[row,5]==2 | y == "" |(tree_matrix[row,6]==2 & draw==1)){
      x<- sentence[tt]
    }else{
      x<- ""
    }
    if (side == "left") {
      left_functions[[row]] <- x
    }else if(side== "right"){
      right_functions[[row]]<- x
    }
    
    if(x!="" & y!=""){
      tree_matrix[row,5]<-2
      tree_matrix[row,6]<-2
    }else if((x!="" & y=="")|(y!=""&x=="")){
      tree_matrix[row,5]<- 1
      tree_matrix[row,6]<- 1
    }
    
    nonterminal<- tree_matrix[row,7]
    if(side=="left"){
    rule<- list(nonterminal, vector(), x, y)
    rule_left<- list(nonterminal,vector(),x)
    rule_right<- list(nonterminal,vector(),y)
    }else if(side=="right"){
    rule<- list(nonterminal,vector(),y,x)
    rule_left<- list(nonterminal,vector(),y)
    rule_right<- list(nonterminal,vector(),x)
    }
    lklh<- lklh*likelihood_emission2(rule,rule_left,rule_right,side,alphabet,proba_emission,proba_epsilon,nonterminals_vec_all,rules,rules_left,rules_right)
    rules[[(length(rules)+1)]]<- rule
    rules_left[[(length(rules_left)+1)]]<- rule_left
    rules_right[[(length(rules_right)+1)]]<- rule_right
    
    if(x==sentence[tt]){
    likelihoods[tt]<- lklh
    tt<- tt+1
    lklh<- 1
    }
    
    tree_matrix<- update_minmax1(rows,tree_matrix)
    row <- rows[(length(rows) - 1)]
    side <- sides[(length(sides) - 1)]
    numb<- number
    number<- numbers[(length(numbers) -1)]
    rows <- rows[1:(length(rows) - 1)]
    sides <- sides[1:(length(sides) - 1)]
    numbers<- numbers[1:(length(numbers) -1)]
    
    if (side == "left") {
      string1 <- left_functions[[row]]
      left_functions[[row]] <- evaluate2(string1,x,numb)
    } else if (side == "right") {
      string1 <- right_functions[[row]]
      right_functions[[row]] <- evaluate2(string1, x, numb)
    }
  }
  
  tree_matrix
  row
  lklh

  
  i<- 1
  j<- 1
  
  vec<- vector(length=length(left_functions[[1]]))
  for(k in 1:length(vec)){
    vec[k]<- is.numeric(left_functions[[1]][[k]]) == FALSE
  }
  if(sum(vec)==length(vec)){i<- length(vec)+1}else{
    while(is.numeric(left_functions[[1]][[i]])==FALSE & i <= length(left_functions[[1]])){i<- i+1}
  }
  
  vec<- vector(length=length(right_functions[[1]]))
  for(k in 1:length(vec)){
    vec[k]<- is.numeric(right_functions[[1]][[k]]) == FALSE
  }
  if(sum(vec)==length(vec)){j<- length(vec)+1}else{
    while(is.numeric(right_functions[[1]][[j]])==FALSE & j <= length(right_functions[[1]])){j<- j+1}
  }
  
  
  if((i > length(left_functions) & j > length(right_functions))|tt>length(sentence)){
    stop<- TRUE
  }
}
tree_matrix
stop
left_functions[[1]]

