remove(list=ls())

setwd("C:/Users/Caroline/Documents/PhD/2MCFG")
source("functions.R")
source("sim_sentences.R")
library(tictoc)
set.seed(9)

tic()

grammar <- "g0"
alpha1 <- 5 #scaling parameter for DP over nonterminals
alpha2 <- 5 #scaling parameter for DP over rules
#gamma <- 1
a1<- 1
a2<- 1
b1<- 1
b2<- 1
c1<- 1
c2<- 1

#proba_emission <- 0.5 #probability of emission
#proba_epsilon<- 0.8 #probability of emission being empty
grammar<- "g0"
#terminals<- c("a","b","c")
#sentence<- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
#sentences<- list()
#number_of_sentences<- 1
#for(i in 1:number_of_sentences){
#sentences[[i]]<- rep(c("a","b","c","d","e"),4)
#}
M<- 5000

list_nonterminals_vec_long<- list()
list_nonterminals_vec_short<- list()
list_p_rules<- list()
list_e_rules<- list()
list_gamma_matrix<- list()
list_type_matrix<- list()
list_epsilon_matrix<- list()
list_tree_matrix<- list()
list_left_functions<- list()
list_right_functions<- list()
list_rows<- list()
list_sides<- list()
list_numbers<- list()

for(i in 1:M){
list_nonterminals_vec_long[[i]]<- 1
list_nonterminals_vec_short[[i]]<- vector()
list_p_rules[[i]]<- list()
list_e_rules[[i]]<- list()
list_gamma_matrix[[i]]<- matrix(c(1,a1,a2),nrow=1,ncol=3)
list_type_matrix[[i]]<- matrix(c(1,b1,b2),nrow=1,ncol=3)
list_epsilon_matrix[[i]]<- matrix(c(1,c1,c2),nrow=1,ncol=3)
#list_tree_matrix[[i]]<- matrix(ncol=8)
#colnames(list_tree_matrix[[i]])<- c("ind_1","ind_2","n_children","type","min","max","B","nrows")
#list_tree_matrix[[i]][1,2]<- 1
#list_tree_matrix[[i]][1, 5]<- length(sentence)
#list_tree_matrix[[i]][1, 6]<- length(sentence)
#list_tree_matrix[[i]][1, 7]<- 1
#list_tree_matrix[[i]][1,8]<- 1
#list_left_functions[[i]]<- list()
#list_right_functions[[i]]<- list()
#list_rows[[i]]<- 1
#list_sides[[i]]<- "left"
#list_numbers[[i]]<- 1
}

list_nonterminals_vec_long1<- list_nonterminals_vec_long
list_nonterminals_vec_short1<- list_nonterminals_vec_short
list_p_rules1<- list_p_rules
list_e_rules1<- list_e_rules
list_gamma_matrix1<- list_gamma_matrix
list_type_matrix1<- list_type_matrix
list_epsilon_matrix1<- list_epsilon_matrix
#list_tree_matrix1<- list_tree_matrix
#list_left_functions1<- list_left_functions
#list_right_functions1<- list_right_functions
#list_rows1<- list_rows
#list_sides1<- list_sides
#list_numbers1<- list_numbers

weights<- rep(1,M)

for(ss in 1:length(sentences)){
  print(ss)
ss<- 1
sentence<- sentences[[ss]]
  
  for(i in 1:M){
    list_tree_matrix[[i]]<- matrix(ncol=8)
    colnames(list_tree_matrix[[i]])<- c("ind_1","ind_2","n_children","type","min","max","B","nrows")
    list_tree_matrix[[i]][1,2]<- 1
    list_tree_matrix[[i]][1, 5]<- length(sentence)
    list_tree_matrix[[i]][1, 6]<- length(sentence)
    list_tree_matrix[[i]][1, 7]<- 1
    list_tree_matrix[[i]][1,8]<- 1
    list_left_functions[[i]]<- list()
    list_right_functions[[i]]<- list()
    list_rows[[i]]<- 1
    list_sides[[i]]<- "left"
    list_numbers[[i]]<- 1
  } 
  list_tree_matrix1<- list_tree_matrix
  list_left_functions1<- list_left_functions
  list_right_functions1<- list_right_functions
  list_rows1<- list_rows
  list_sides1<- list_sides
  list_numbers1<- list_numbers

for(ttt in 1:length(sentence)){

  for(i in 1:M){
  tt<- ttt
  nonterminals_vec_long<- list_nonterminals_vec_long[[i]]
  nonterminals_vec_short<- list_nonterminals_vec_short[[i]]
  p_rules<- list_p_rules[[i]]
  e_rules<- list_e_rules[[i]]
  gamma_matrix<- list_gamma_matrix[[i]]
  type_matrix<- list_type_matrix[[i]]
  epsilon_matrix<- list_epsilon_matrix[[i]]
  tree_matrix<- list_tree_matrix[[i]]
  left_functions<- list_left_functions[[i]]
  right_functions<- list_right_functions[[i]]
  rows<- list_rows[[i]]
  sides<- list_sides[[i]]
  numbers<- list_numbers[[i]]
  row<- tail(rows,1)
  side<- tail(sides,1)
  number<- tail(numbers,1)
  w<- 1
  
  while(tt == ttt){
    if(is.na(tree_matrix[row,4])){#then draw a rule
      
      tree_matrix[row,8]<- nrow(tree_matrix)
      minimum<- tree_matrix[row, 5] #minimal number of terminal symbols associated with current nonterminal
      maximum<- tree_matrix[row, 6] #maximal number of terminal symbols associated with current nonterminal
      nonterminal<- tree_matrix[row, 7] #current nonterminal symbol
      
      if(length(which(gamma_matrix[,1] == nonterminal))==0){
      gamma_matrix<- rbind(gamma_matrix,c(nonterminal,1,1))
      type_matrix<- rbind(type_matrix,c(nonterminal,1,1))
      epsilon_matrix<- rbind(epsilon_matrix,c(nonterminal,1,1))
      }
      
      new_rule<- dp_random(nonterminal,minimum,maximum)
      
      rule<- new_rule[[1]]
      type<- new_rule[[2]]
      ww<- new_rule[[3]]
      tree_matrix[row,4]<- type
      w<- w*ww
      
      index<- which(type_matrix[,1]==nonterminal)
      if(type == 0|type==1){
      type_matrix[index,3]<- type_matrix[index,3]+1
      }else if(type==2|type==3){
      type_matrix[index,2]<- type_matrix[index,2]+1
      if(type==2 & rule!=""){
      epsilon_matrix[index,3]<- epsilon_matrix[index,3]+1
      }else{
      epsilon_matrix[index,2]<- epsilon_matrix[index,2]+1
      }
      }
      
      
      if (type == 0 | type ==1){#production rule
        
        index<- which(gamma_matrix[,1]==nonterminal)
        gamma_matrix[index,2]<- gamma_matrix[index,2]+rule[[5]]
        gamma_matrix[index,3]<- gamma_matrix[index,3]+1
        
        left_functions[[row]] <- as.list(rule[[3]])
        right_functions[[row]] <- as.list(rule[[4]])
        p_rules[[length(p_rules)+1]]<- rule
        nonterminals_vec_short<- c(nonterminals_vec_short,nonterminal)
        nonterminals_vec_long<- c(nonterminals_vec_long,rule[[2]])
        
        tree_matrix[row, 3] <- rule[[5]]
        nc <- rule[[5]] #number of children
        nr <- nrow(tree_matrix) #number of rows
        if (nc == 1) {
          tree_matrix <-rbind(tree_matrix, c(row, nr + 1, rep(NA, 2), minimum, maximum, rule[[2]][1],nr+nc))
          left_functions[[nr + 1]] <-list()
          right_functions[[nr + 1]] <-list()
        }else{
          for(cc in 1:nc){
            tree_matrix <-rbind(tree_matrix, c(row, nr + cc, rep(NA, 2), 1, maximum - nc + 1, rule[[2]][cc],nr+nc))
            left_functions[[nr + cc]] <- list()
            right_functions[[nr + cc]] <- list()
          }
          minimum<- max(tree_matrix[row,5],nc)
          tree_matrix[row,5]<- minimum
          tree_matrix<- update("min")
        }
      }else if(type == 2 | type== 3){#emission rule
        
        tree_matrix[row,3]<- 0
        x<- rule
        
        if(type == 2 & x != ""){
          maximum<- 2
          tree_matrix[row,6]<- maximum
          tree_matrix<- update("max")
          minimum<- 2
          tree_matrix[row,5]<- minimum
          tree_matrix<- update("min")
        }else{
          maximum<- 1
          tree_matrix[row,6]<- 1
          tree_matrix<- update("max")
        }
        
        if (side == "left") {
          left_functions[[row]] <- x
          right_functions[[row]]<- list()
        }else if(side=="right"){
          left_functions[[row]]<- list()
          right_functions[[row]]<- x
        }
        
        if(x==sentence[tt]){
          tt<- tt+1
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
            left_functions[[row]] <- evaluate(string1,x,numb)
          } else if (side == "right") {
            string1 <- right_functions[[row]]
            right_functions[[row]] <- evaluate(string1, x, numb)
          }
        }else{
          if(side=="left"){
            side1<- "right"
            sides1<- "right"
          }
          side<- side1
          sides<- sides1
        }
      }
    }else if(tree_matrix[row,4]==0| tree_matrix[row,4]==1){
      
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
            left_functions[[row]] <- evaluate(string1, x, numb)
          } else if (side == "right") {
            string1 <- right_functions[[row]]
            right_functions[[row]] <- evaluate(string1, x, numb)
          }
          
        }else{
          
          ii<- 1
          while(is.numeric(string[[ii]])==FALSE){ii<- ii+1}
          
          number <- string[[ii]]
          if (ceiling(number / 2) == number / 2) {
            side <- "right"
          } else if (ceiling(number / 2) != number / 2) {
            side <- "left"
          }
          row <- tree_matrix[row,8] + ceiling(number / 2)
          sides <- c(sides, side)
          rows <- c(rows, row)
          numbers<- c(numbers, number)
          
          
          
        }
      }
    }else if(tree_matrix[row,4]==2| tree_matrix[row,4]==3){
      
      nonterminal<- tree_matrix[row,7]
      minimum<- tree_matrix[row,5]
      maximum<- tree_matrix[row,6]
      type<- tree_matrix[row,4]
      
      if(type == 2){
        x<- sentence[tt]
      }else{
        x<- ""
      }
      
      if (side == "left") {
        left_functions[[row]] <- x
      }else if(side== "right"){
        right_functions[[row]]<- x
      }
      
      complete_rule<- c(nonterminal, left_functions[[row]],right_functions[[row]])
      e_rules[[length(e_rules)+1]]<- complete_rule
      
      if(x==sentence[tt]){
        tt<- tt+1
      }
      
      if(length(rows)!=1){
        row <- rows[(length(rows) - 1)]
        side <- sides[(length(sides) - 1)]
        numb<- number
        number<- numbers[(length(numbers) -1)]
        rows <- rows[1:(length(rows) - 1)]
        sides <- sides[1:(length(sides) - 1)]
        numbers<- numbers[1:(length(numbers) -1)]
        
        if (side == "left") {
          string1 <- left_functions[[row]]
          left_functions[[row]] <- evaluate(string1,x,numb)
        } else if (side == "right") {
          string1 <- right_functions[[row]]
          right_functions[[row]] <- evaluate(string1, x, numb)
        }
      }
    }
  }# while tt == ttt
 
  list_nonterminals_vec_long[[i]]<- nonterminals_vec_long
  list_nonterminals_vec_short[[i]]<- nonterminals_vec_short
  list_p_rules[[i]]<- p_rules
  list_e_rules[[i]]<- e_rules
  list_gamma_matrix[[i]]<- gamma_matrix
  list_type_matrix[[i]]<- type_matrix
  list_epsilon_matrix[[i]]<- epsilon_matrix
  list_tree_matrix[[i]]<- tree_matrix
  list_left_functions[[i]]<- left_functions
  list_right_functions[[i]]<- right_functions
  list_rows[[i]]<- rows
  list_sides[[i]]<- sides
  list_numbers[[i]]<- numbers
  weights[i]<- w
  
  
}#for i in 1:M   
  
  particles<- sample(1:M,M,weights,replace=TRUE)
  
  for(i in 1:M){
    j<- particles[i]
    list_nonterminals_vec_long1[[i]]<- list_nonterminals_vec_long[[j]]
    list_nonterminals_vec_short1[[i]]<- list_nonterminals_vec_short[[j]]
    list_p_rules1[[i]]<- list_p_rules[[j]]
    list_e_rules1[[i]]<- list_e_rules[[j]]
    list_gamma_matrix1[[i]]<- list_gamma_matrix[[j]]
    list_type_matrix1[[i]]<- list_type_matrix[[j]]
    list_epsilon_matrix1[[i]]<- list_epsilon_matrix[[j]]
    list_tree_matrix1[[i]]<- list_tree_matrix[[j]]
    list_left_functions1[[i]]<- list_left_functions[[j]]
    list_right_functions1[[i]]<- list_right_functions[[j]]
    list_rows1[[i]]<- list_rows[[j]]
    list_sides1[[i]]<- list_sides[[j]]
    list_numbers1[[i]]<- list_numbers[[j]]
  }
  for(i in 1:M){
    list_nonterminals_vec_long[[i]]<- list_nonterminals_vec_long1[[i]]
    list_nonterminals_vec_short[[i]]<- list_nonterminals_vec_short1[[i]]
    list_p_rules[[i]]<- list_p_rules1[[i]]
    list_e_rules[[i]]<- list_e_rules1[[i]]
    list_gamma_matrix[[i]]<- list_gamma_matrix1[[i]]
    list_type_matrix[[i]]<- list_type_matrix1[[i]]
    list_epsilon_matrix[[i]]<- list_epsilon_matrix1[[i]]
    list_tree_matrix[[i]]<- list_tree_matrix1[[i]]
    list_left_functions[[i]]<- list_left_functions1[[i]]
    list_right_functions[[i]]<- list_right_functions1[[i]]
    list_rows[[i]]<- list_rows1[[i]]
    list_sides[[i]]<- list_sides1[[i]]
    list_numbers[[i]]<- list_numbers1[[i]]
  }
  weights1<- weights
  weights<- rep(1,M)
  
}#ttt



for(i in 1:M){
  nonterminals_vec_long<- list_nonterminals_vec_long[[i]]
  nonterminals_vec_short<- list_nonterminals_vec_short[[i]]
  p_rules<- list_p_rules[[i]]
  e_rules<- list_e_rules[[i]]
  gamma_matrix<- list_gamma_matrix[[i]]
  type_matrix<- list_type_matrix[[i]]
  epsilon_matrix<- list_epsilon_matrix[[i]]
  tree_matrix<- list_tree_matrix[[i]]
  left_functions<- list_left_functions[[i]]
  right_functions<- list_right_functions[[i]]
  rows<- list_rows[[i]]
  sides<- list_sides[[i]]
  numbers<- list_numbers[[i]]
  row<- tail(rows,1)
  side<- tail(sides,1)
  number<- tail(numbers,1)
  w<- 1
  
  count<- 0
  if(length(left_functions[[1]])>0){
    for(j in 1:length(left_functions[[1]])){
      count<- count+ is.numeric(left_functions[[1]][[j]])}
  }
  if(length(right_functions[[1]])>0){
    for(j in 1:length(right_functions[[1]])){
      count<- count+ is.numeric(right_functions[[1]][[j]])}
  }
  

while(count > 0){
  
  if(tree_matrix[row,4]==0| tree_matrix[row,4]==1){
    
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
          left_functions[[row]] <- evaluate(string1, x, numb)
        } else if (side == "right") {
          string1 <- right_functions[[row]]
          right_functions[[row]] <- evaluate(string1, x, numb)
        }
        
      }else{
        
        ii<- 1
        while(is.numeric(string[[ii]])==FALSE){ii<- ii+1}
        
        number <- string[[ii]]
        if (ceiling(number / 2) == number / 2) {
          side <- "right"
        } else if (ceiling(number / 2) != number / 2) {
          side <- "left"
        }
        row <- tree_matrix[row,8] + ceiling(number / 2)
        sides <- c(sides, side)
        rows <- c(rows, row)
        numbers<- c(numbers, number)
        
        
        
      }
    }
  }else if(tree_matrix[row,4]==2| tree_matrix[row,4]==3){
    
    nonterminal<- tree_matrix[row,7]
    minimum<- tree_matrix[row,5]
    maximum<- tree_matrix[row,6]
    type<- tree_matrix[row,4]
    
    if(type == 2){
      x<- sentence[tt]
    }else{
      x<- ""
    }
    
    if (side == "left") {
      left_functions[[row]] <- x
    }else if(side== "right"){
      right_functions[[row]]<- x
    }
    
    complete_rule<- c(nonterminal, left_functions[[row]],right_functions[[row]])
    e_rules[[length(e_rules)+1]]<- complete_rule
    
    if(length(rows)!=1){
      row <- rows[(length(rows) - 1)]
      side <- sides[(length(sides) - 1)]
      numb<- number
      number<- numbers[(length(numbers) -1)]
      rows <- rows[1:(length(rows) - 1)]
      sides <- sides[1:(length(sides) - 1)]
      numbers<- numbers[1:(length(numbers) -1)]
      
      if (side == "left") {
        string1 <- left_functions[[row]]
        left_functions[[row]] <- evaluate(string1,x,numb)
      } else if (side == "right") {
        string1 <- right_functions[[row]]
        right_functions[[row]] <- evaluate(string1, x, numb)
      }
    }
  }
  
  count1<- 0
  if(length(left_functions[[1]])>0){
    for(j in 1:length(left_functions[[1]])){
      count1<- count1+ is.numeric(left_functions[[1]][[j]])}
  }
  if(length(right_functions[[1]])>0){
    for(j in 1:length(right_functions[[1]])){
      count1<- count1+ is.numeric(right_functions[[1]][[j]])}
  }
  count<- count1
}
  
  list_nonterminals_vec_long[[i]]<- nonterminals_vec_long
  list_nonterminals_vec_short[[i]]<- nonterminals_vec_short
  list_p_rules[[i]]<- p_rules
  list_e_rules[[i]]<- e_rules
  list_gamma_matrix[[i]]<- gamma_matrix
  list_type_matrix[[i]]<- type_matrix
  list_epsilon_matrix[[i]]<- epsilon_matrix
  list_tree_matrix[[i]]<- tree_matrix
  list_left_functions[[i]]<- left_functions
  list_right_functions[[i]]<- right_functions
  list_rows[[i]]<- rows
  list_sides[[i]]<- sides
  list_numbers[[i]]<- numbers
  weights[i]<- w
  
}

particles<- sample(1:M,M,weights,replace=TRUE)
for(i in 1:M){
  j<- particles[i]
  list_nonterminals_vec_long1[[i]]<- list_nonterminals_vec_long[[j]]
  list_nonterminals_vec_short1[[i]]<- list_nonterminals_vec_short[[j]]
  list_p_rules1[[i]]<- list_p_rules[[j]]
  list_e_rules1[[i]]<- list_e_rules[[j]]
  list_gamma_matrix1[[i]]<- list_gamma_matrix[[j]]
  list_type_matrix1[[i]]<- list_type_matrix[[j]]
  list_epsilon_matrix1[[i]]<- list_epsilon_matrix[[j]]
  list_tree_matrix1[[i]]<- list_tree_matrix[[j]]
  list_left_functions1[[i]]<- list_left_functions[[j]]
  list_right_functions1[[i]]<- list_right_functions[[j]]
  list_rows1[[i]]<- list_rows[[j]]
  list_sides1[[i]]<- list_sides[[j]]
  list_numbers1[[i]]<- list_numbers[[j]]
}
for(i in 1:M){
  list_nonterminals_vec_long[[i]]<- list_nonterminals_vec_long1[[i]]
  list_nonterminals_vec_short[[i]]<- list_nonterminals_vec_short1[[i]]
  list_p_rules[[i]]<- list_p_rules1[[i]]
  list_e_rules[[i]]<- list_e_rules1[[i]]
  list_gamma_matrix[[i]]<- list_gamma_matrix1[[i]]
  list_type_matrix[[i]]<- list_type_matrix1[[i]]
  list_epsilon_matrix[[i]]<- list_epsilon_matrix1[[i]]
  list_tree_matrix[[i]]<- list_tree_matrix1[[i]]
  list_left_functions[[i]]<- list_left_functions1[[i]]
  list_right_functions[[i]]<- list_right_functions1[[i]]
  list_rows[[i]]<- list_rows1[[i]]
  list_sides[[i]]<- list_sides1[[i]]
  list_numbers[[i]]<- list_numbers1[[i]]
}
weights<- rep(1,M)

}##ss 1:length(sentences)




production_count<- vector(length=M)
emission_count<- vector(length= M)

for(i in 1:M){
  production_count[i]<- length(list_p_rules[[i]])
  emission_count[i]<- length(list_e_rules[[i]])
}
production_count
emission_count

nonterminals_vec_long

toc()
p_rules
