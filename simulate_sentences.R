source("functions.R")
source("functions2.R")
library(seqinr)
C_rules<- 0 #factor to add to each of the observed rules
C_nonterminals<- 0 #factor to add to each of the observed nonterminals
terminals<- c("a","b","c")

Q<- 1000 #number of sentences to generate
sim_sentences<- list()
sentence_length<- vector(length=Q)
edit_distance<- vector(length = Q)
qq<- 0
for(QQ in 1:Q){
  print(paste0("QQ=",QQ))
  #### construct a tree
  
  stop<- TRUE
  while(stop == TRUE){
    
    stop<- FALSE
    tree_matrix<- matrix(ncol=8)
    colnames(tree_matrix)<- c("ind_1","ind_2","n_children","type","min","max","B","nrows")
    tree_matrix[1,2]<- 1
    tree_matrix[1, 5]<- 3
    tree_matrix[1, 6]<- 100
    tree_matrix[1, 7]<- 1
    tree_matrix[1,8]<- 1
    left_functions<- list()
    right_functions<- list()
    
    row_vec<- tree_matrix[,3]
    while(sum(is.na(row_vec))>0 & stop == FALSE){
      row<- which(is.na(row_vec))[1]
      rows<- row
      row1<- row
      while(is.na(tree_matrix[row1,1])==FALSE){
        row1<- tree_matrix[row1,1]
        rows<- c(row1,rows)
      }
      nonterminal<- tree_matrix[row,7]
      minimum<- tree_matrix[row,5]
      maximum<- tree_matrix[row,6]
      
      if(length(which(type_matrix[,1] == nonterminal))==0){
        type_matrix<- rbind(type_matrix,c(nonterminal,b1,b2))
        epsilon_matrix<- rbind(epsilon_matrix,c(nonterminal,c1,c2))
      }
      if(length(which(terminals_matrix[,1] == nonterminal))==0){
        terminals_matrix<- rbind(terminals_matrix,c(nonterminal,rep(1,length(terminals))))
      }
      
      rule_all<- dp_random2(nonterminal, minimum, maximum)
      rule<- rule_all[[1]]
      stop<- rule_all[[2]]
      draws<- rule_all[[3]]
      permutation1<- rule_all[[4]]
      rule_index<- rule_all[[5]]
      
      if(draws[2]==0){#production
        tree_matrix[row, 3]<- 2
        left_functions[[row]]<- as.list(rule[[3]])
        right_functions[[row]]<- as.list(rule[[4]])
        nc <- 2 #number of children
        nr <- nrow(tree_matrix) #number of rows
        for(cc in 1:nc){
          tree_matrix <-rbind(tree_matrix, c(row, nr + cc, rep(NA, 2), 1, maximum - nc + 1, rule[[2]][cc],nr+nc))
          left_functions[[nr + cc]] <- list()
          right_functions[[nr + cc]] <- list()
        }
        minimum<- max(tree_matrix[row,5],nc)
        tree_matrix[row,5]<- minimum
        tree_matrix<- update("min")
      }else if(draws[2]==1){#emission
        
        tree_matrix[row, 3]<- 0
        left_functions[[row]]<- rule[2]
        right_functions[[row]]<- rule[3]
        if(rule[2]==""|rule[3]==""){
          maximum<- 1
          tree_matrix[row,6]<- maximum
          tree_matrix<- update("max")
        }else{
          maximum<- 2
          tree_matrix[row,6]<- maximum
          tree_matrix<- update("max")
          minimum<- 2
          tree_matrix[row,5]<- minimum
          tree_matrix<- update("min")
        }
      }
      
      row_vec<- tree_matrix[,3]
      tree_matrix
      
    }
  }
  
  ###evaluate the tree
  
  row<- 1
  side<- "left"
  number<- 1
  rows<- row
  sides<- side
  numbers<- number
  string<- left_functions[[row]]
  
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
    
    if(tree_matrix[row,3]==2){
      
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
    }else if(tree_matrix[row,3]==0){
      
      if(side == "left"){
        x<- left_functions[[row]]
      }else{
        x<- right_functions[[row]]
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
  
  simulated_sentence<- vector(length = 0)
  
  for(i in 1:length(left_functions[[1]])){
    words<- left_functions[[1]][[i]]
    words_sep<- s2c(words)
    simulated_sentence<- c(simulated_sentence,words_sep)
  }
  for(i in 1:length(right_functions[[1]])){
    words<- right_functions[[1]][[i]]
    words_sep<- s2c(words)
    simulated_sentence<- c(simulated_sentence,words_sep)
  }
  if(length(which(simulated_sentence == ""))>0){
    simulated_sentence<- simulated_sentence[-(which(simulated_sentence == ""))]
  }
  if(length(which(is.na(simulated_sentence)))>0){
    simulated_sentence<- simulated_sentence[-(which(is.na(simulated_sentence)))]
  }
  
  if(g=="copy" | g=="mix"){
    cut<- round(length(simulated_sentence)/2)
    s1<- simulated_sentence[1:cut]
    s2<- simulated_sentence[(cut+1):length(simulated_sentence)]
    s1<- paste0(s1,collapse = "")
    s2<- paste0(s2,collapse = "")
    if(s1 == s2){qq<- qq+1}
    #edit_distance[QQ]<- adist(s1,s2)
  }else if(g=="doubles" | g=="mix"){
    s1<- simulated_sentence[seq(1,length(simulated_sentence),2)]
    s2<- simulated_sentence[seq(2,length(simulated_sentence),2)]
    z = rle(rle(simulated_sentence)$lengths %% 2)
    edit_distance[[QQ]]<- sum(ceiling(z$lengths[z$values == 1] / 2))
    s1<- paste0(s1,collapse = "")
    s2<- paste0(s2,collapse = "")
    if(s1 == s2){qq<- qq+1}
  }else if(g=="monkey"){
    s1<- 1
    s2<- 2
  }
  sentence_length[QQ]<- length(simulated_sentence)
  sim_sentences[[length(sim_sentences)+1]]<- simulated_sentence

}
#print(description)
proportion<- qq/Q
print("proportion of correct estimates:")
print(proportion)
