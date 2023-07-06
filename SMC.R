remove(list=ls())

library(tictoc)
tic()
source("functions.R")
terminals<- c("a","b","c")

g<- "copy"
M<- 1
number_sentences<- 10

number_sentences1<- round(number_sentences/6)
number_sentences2<- round(number_sentences/6)
number_sentences3<- round(number_sentences/6)
number_sentences4<- number_sentences - number_sentences1 - number_sentences2 - number_sentences3
#number_sentences1 <- number_sentences2 <- number_sentences3 <- 0
#number_sentences4<- number_sentences
len1<- 4
len2<- 6
len3<- 8
len4<- 12
description<- paste0("G=",g,"_M=",M,"_S=",number_sentences)
filename<- paste0(Sys.Date(),"_",description)
sentences<- list()

for(i in 1:number_sentences1){
  sent_short<- sample(terminals,len1/2,replace = TRUE)
  if(g=="copy"){
    sentences[[length(sentences)+1]]<- rep(sent_short,2)
  }else if(g=="doubles"){
    sentences[[length(sentences)+1]]<- rep(sent_short,each=2)
  }
}

for(i in 1:number_sentences2){
  sent_short<- sample(terminals,len2/2,replace = TRUE)
  if(g=="copy"){
    sentences[[length(sentences)+1]]<- rep(sent_short,2)
  }else if(g=="doubles"){
    sentences[[length(sentences)+1]]<- rep(sent_short,each=2)
  }
}

for(i in 1:number_sentences3){
  sent_short<- sample(terminals,len3/2,replace = TRUE)
  if(g=="copy"){
    sentences[[length(sentences)+1]]<- rep(sent_short,2)
  }else if(g=="doubles"){
    sentences[[length(sentences)+1]]<- rep(sent_short,each=2)
  }
}

for(i in 1:number_sentences4){
  sent_short<- sample(terminals,len4/2,replace = TRUE)
  if(g=="copy"){
    sentences[[length(sentences)+1]]<- rep(sent_short,2)
  }else if(g=="doubles"){
    sentences[[length(sentences)+1]]<- rep(sent_short,each=2)
  }
}

sentence<- sentences[[1]]
sent= paste(sentence,collapse="")

C_rules<- 0 #factor to add to each of the observed rules
C_nonterminals<- 0 #factor to add to each of the observed nonterminals
alpha1 <- 50 #scaling parameter for DP over nonterminals
alpha2 <- 50 #scaling parameter for DP over rules
a1<- 1 #Gamma parameters for poisson
a2<- 1
b1<- 1000 #Beta parameters for type = emission
b2<- 1
c1<- 1 #Beta parameters for epsilon
c2<- 1000
permutation_parameters<- rep(1,factorial(5))
if(g == "copy"){
  permutation_parameters[51]<- 119
}else if(g == "doubles"){
  permutation_parameters[49]<- 119
}


grammar<- "g0"
#grammar<- "cf"



list_nonterminals_vec_long<- list()
list_nonterminals_vec_short<- list()
list_p_rules<- list()
list_e_rules<- list()
list_gamma_matrix<- list()
list_type_matrix<- list()
list_epsilon_matrix<- list()
list_terminals_matrix<- list()
list_tree_matrix<- list()
list_left_functions<- list()
list_right_functions<- list()
list_rows<- list()
list_sides<- list()
list_numbers<- list()


list_max_nonterminals<- list()
vec_max_nonterminals<- vector(length = M)

for(i in 1:M){
  list_nonterminals_vec_long[[i]]<- 1
  list_nonterminals_vec_short[[i]]<- vector()
  list_p_rules[[i]]<- list()
  list_e_rules[[i]]<- list()
  list_gamma_matrix[[i]]<- matrix(c(1,a1,a2),nrow=1,ncol=3)
  list_type_matrix[[i]]<- matrix(c(1,b1,b2),nrow=1,ncol=3)
  list_epsilon_matrix[[i]]<- matrix(c(1,c1,c2),nrow=1,ncol=3)
  list_terminals_matrix[[i]]<- matrix(1,nrow=1,ncol=length(terminals)+1)
}

list_nonterminals_vec_long1<- list_nonterminals_vec_long
list_nonterminals_vec_short1<- list_nonterminals_vec_short
list_p_rules1<- list_p_rules
list_e_rules1<- list_e_rules
list_gamma_matrix1<- list_gamma_matrix
list_type_matrix1<- list_type_matrix
list_epsilon_matrix1<- list_epsilon_matrix
list_terminals_matrix1<- list_terminals_matrix


weights<- rep(1,M)

for(ss in 1:length(sentences)){
  print(paste0("ss=",ss))
  
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
    #print(ttt)
    for(i in 1:M){
      tt<- ttt
      nonterminals_vec_long<- list_nonterminals_vec_long[[i]]
      nonterminals_vec_short<- list_nonterminals_vec_short[[i]]
      p_rules<- list_p_rules[[i]]
      e_rules<- list_e_rules[[i]]
      gamma_matrix<- list_gamma_matrix[[i]]
      type_matrix<- list_type_matrix[[i]]
      epsilon_matrix<- list_epsilon_matrix[[i]]
      terminals_matrix<- list_terminals_matrix[[i]]
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
            gamma_matrix<- rbind(gamma_matrix,c(nonterminal,a1,a2))
            type_matrix<- rbind(type_matrix,c(nonterminal,b1,b2))
            epsilon_matrix<- rbind(epsilon_matrix,c(nonterminal,c1,c2))
          }
          
          new_rule<- dp_random(nonterminal,minimum,maximum)
          
          rule<- new_rule[[1]]
          type<- new_rule[[2]]
          ww<- new_rule[[3]]
          p_rules<- new_rule[[4]]
          permutation<- new_rule[[5]]
          if(permutation>0){
            permutation_parameters[permutation]<- permutation_parameters[permutation]+1
          }
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
            if(rule[[6]]==1){
              p_rules[[length(p_rules)+1]]<- rule
              nonterminals_vec_short<- c(nonterminals_vec_short,nonterminal)
            }
            
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
          
          if(length(which(terminals_matrix[,1] == nonterminal))==0){
            terminals_matrix<- rbind(terminals_matrix,c(nonterminal,rep(1,length(terminals))))
          }
          
          if(type == 2){
            x<- sentence[tt]
          }else{
            x<- ""
          }
          
          if (side == "left") {
            left_functions[[row]] <- x
            y<- right_functions[[row]]
          }else if(side== "right"){
            right_functions[[row]]<- x
            y<- left_functions[[row]]
          }
          
          complete_rule<- c(nonterminal, left_functions[[row]],right_functions[[row]])
          e_rules[[length(e_rules)+1]]<- complete_rule
          
          w<- w*weight_e2(nonterminal)
          
          ind_nt<- which(terminals_matrix[,1]==nonterminal)
          if(x!=""){
            ind_x<- which(terminals ==x)
            terminals_matrix[ind_nt,(ind_x+1)]<-  terminals_matrix[ind_nt,(ind_x+1)]+1
          }
          if(y!=""){
            ind_y<- which(terminals ==y)
            terminals_matrix[ind_nt,(ind_y+1)]<-  terminals_matrix[ind_nt,(ind_y+1)]+1
          }
          
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
      list_terminals_matrix[[i]]<- terminals_matrix
      list_tree_matrix[[i]]<- tree_matrix
      list_left_functions[[i]]<- left_functions
      list_right_functions[[i]]<- right_functions
      list_rows[[i]]<- rows
      list_sides[[i]]<- sides
      list_numbers[[i]]<- numbers
      weights[i]<- w
      
     vec_max_nonterminals[i]<- max(nonterminals_vec_long) 
    }#for i in 1:M   
    list_max_nonterminals[[length(list_max_nonterminals)+1]]<- vec_max_nonterminals
    
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
      list_terminals_matrix1[[i]]<- list_terminals_matrix[[j]]
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
      list_terminals_matrix[[i]]<- list_terminals_matrix1[[i]]
      list_tree_matrix[[i]]<- list_tree_matrix1[[i]]
      list_left_functions[[i]]<- list_left_functions1[[i]]
      list_right_functions[[i]]<- list_right_functions1[[i]]
      list_rows[[i]]<- list_rows1[[i]]
      list_sides[[i]]<- list_sides1[[i]]
      list_numbers[[i]]<- list_numbers1[[i]]
    }
    weights1<- weights
    weights<- rep(1,M)
    
    
  }#ttt in 1:length(sentence)
  
  
  
  for(i in 1:M){
    nonterminals_vec_long<- list_nonterminals_vec_long[[i]]
    nonterminals_vec_short<- list_nonterminals_vec_short[[i]]
    p_rules<- list_p_rules[[i]]
    e_rules<- list_e_rules[[i]]
    gamma_matrix<- list_gamma_matrix[[i]]
    type_matrix<- list_type_matrix[[i]]
    epsilon_matrix<- list_epsilon_matrix[[i]]
    terminals_matrix<- list_terminals_matrix[[i]]
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
        
        if(length(which(terminals_matrix[,1] == nonterminal))==0){
          terminals_matrix<- rbind(terminals_matrix,c(nonterminal,rep(1,length(terminals))))
        }
        
        if(type == 2){
          x<- sentence[tt]
        }else{
          x<- ""
        }
        
        if (side == "left") {
          left_functions[[row]] <- x
          y<- right_functions[[row]]
        }else if(side== "right"){
          right_functions[[row]]<- x
          y<- left_functions[[row]]
        }
        
        complete_rule<- c(nonterminal, left_functions[[row]],right_functions[[row]])
        e_rules[[length(e_rules)+1]]<- complete_rule
        
        w<- w*weight_e2(nonterminal)
        
        ind_nt<- which(terminals_matrix[,1]==nonterminal)
        if(x!=""){
          ind_x<- which(terminals ==x)
          terminals_matrix[ind_nt,(ind_x+1)]<-  terminals_matrix[ind_nt,(ind_x+1)]+1
        }
        if(y!=""){
          ind_y<- which(terminals ==y)
          terminals_matrix[ind_nt,(ind_y+1)]<-  terminals_matrix[ind_nt,(ind_y+1)]+1
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
    list_terminals_matrix[[i]]<- terminals_matrix
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
    list_terminals_matrix1[[i]]<- list_terminals_matrix[[j]]
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
    list_terminals_matrix[[i]]<- list_terminals_matrix1[[i]]
    list_tree_matrix[[i]]<- list_tree_matrix1[[i]]
    list_left_functions[[i]]<- list_left_functions1[[i]]
    list_right_functions[[i]]<- list_right_functions1[[i]]
    list_rows[[i]]<- list_rows1[[i]]
    list_sides[[i]]<- list_sides1[[i]]
    list_numbers[[i]]<- list_numbers1[[i]]
  }
  weights<- rep(1,M)
  
}##ss 1:length(sentences)

#production_count<- vector(length=M)
#emission_count<- vector(length= M)

#for(i in 1:M){
#  production_count[i]<- length(list_p_rules[[i]])
#  emission_count[i]<- length(list_e_rules[[i]])
#}
#production_count
#emission_count


#table(nonterminals_vec_long)
#e_rules



#proportion1<- vector(length = length(list_e_rules))
#proportion2<- vector(length = length(list_e_rules))
#for(j in 1:length(list_e_rules)){
#  e_rules1<- list_e_rules[[j]]
#  count1<- 0
#  count2<- 0
#  for(i in 1:length(e_rules1)){
#    rule<- e_rules1[[i]]
#    left<- rule[[2]]
#    right<- rule[[3]]
#    if(left == right){count2<- count2+1}
#    if(left == right | left == "" | right ==""){count1 <- count1+1}
#  }
#  proportion1[j]<-count1/length(e_rules1)
#  proportion2[j]<-count2/length(e_rules1)
#}

#prop1<- length(which(proportion1==1))/length(list_e_rules)
#prop2<- length(which(proportion2==1))/length(list_e_rules)

list_grammars_all<- list()
for(i in 1:M){
  list_grammars_all[[length(list_grammars_all)+1]]<- list(list_tree_matrix[[i]],list_left_functions[[i]], list_right_functions[[i]],list_e_rules[[i]],list_p_rules[[i]]) 
}
names(list_grammars_all)<- c("tree","left","right","emissions","productions")

x<- list_grammars_all
unique_grammars_ordered <- function(x) {
  ux <- unique(x)
  tab<- tabulate(match(x,ux))
  index_ux<-order(tab,decreasing=TRUE)
  ux_ordered<- ux[index_ux]
  frequencies<- sort(tab,decreasing = TRUE)
  return(ux_ordered)
}

unique_grammars_frequencies<- function(x){
  ux <- unique(x)
  tab<- tabulate(match(x,ux))
  frequencies<- sort(tab,decreasing = TRUE)
  return(frequencies)
}

ug<- unique_grammars_ordered(list_grammars_all)
ug_frequencies<- unique_grammars_frequencies(list_grammars_all)
print(paste0("number of grammars: ",length(ug_frequencies)))


if(length(ug_frequencies) >=10){
  print("First 10 frequencies:")
  print(ug_frequencies[1:10])
}
mode1<- ug[[1]]

print("Mode tree")
print(mode1[[1]])

print("Mode1 emission rules:")
if(length(mode1[[4]])>=20){
  for(i in 1:20)
    print(mode1[[4]][[i]])
}else{
  print(mode1[[4]])
}

print("Mode1 production rules:")
if(length(mode1[[5]])>=3){
  print(mode1[[5]][[1]])
  print(mode1[[5]][[2]])
  print(mode1[[5]][[3]])
}else{
  print(mode1[[5]])
}

count<- 0
count2<- 0
for(i in 1:length(mode1[[4]])){
  e_rule<- mode1[[4]][[i]]
  if(e_rule[[2]] == e_rule[[3]]){
    count<- count + 1
  }
  if(e_rule[[2]]!=""& e_rule[[3]]!=""){
    count2<- count2 + 1
  }
}
prop_pairs<- count/count2
print(paste0("Mode1 proportion of double emissions=",prop_pairs))

toc()
print(description)

r_object <- list()

r_object[[1]]<- list_nonterminals_vec_long
r_object[[2]]<- list_nonterminals_vec_short
r_object[[3]]<- list_p_rules
r_object[[4]]<- list_e_rules
r_object[[5]]<- list_gamma_matrix
r_object[[6]]<- list_type_matrix
r_object[[7]]<- list_epsilon_matrix
r_object[[8]]<- list_terminals_matrix
r_object[[9]]<- list_tree_matrix
r_object[[10]]<- list_left_functions
r_object[[11]]<- list_right_functions
r_object[[12]]<- list_rows
r_object[[13]]<- list_sides
r_object[[14]]<- list_numbers
r_object[[15]]<- description
r_object[[16]]<- sentences
r_object[[17]]<- permutation_parameters

save(r_object,file=filename)
