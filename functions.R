remove(list=ls())

##Draw from the DP of nonterminals ##returns a vector of length n of nonterminals
draw_nt<- function(alpha1,nonterminals_vec,n){
  nt<- vector(length=n)
  ntv<-nonterminals_vec
  for(i in 1:n){
    if(length(ntv)==0){
      nt[i]<- 1
    }else{
      draw<- sample(c("new","old"),1,prob=c(alpha1,length(ntv)))
      if(draw=="old"){
        s<- sample(1:length(ntv),1)
        nt[i]<- ntv[s]
      }else if(draw=="new"){
        nt[i]<- max(ntv)+1
      }
    }
    ntv<- c(ntv,nt[i])
  }
  return(nt)
}

##Random base production rule
base_production_random<- function(gamma1,alpha1,nonterminals_vec,terminals,grammar){
  if(grammar=="g0"){
    N<- rpois(1,gamma1)+1
    sigma<- sample(1:(2*N),2*N,replace=FALSE)
    cut<- sample(1:(2*N+1),1)
    if(cut==1){
      string1<- NA
      string2<- sigma
    }else if(cut==(2*N+1)){
      string1<- sigma
      string2<- NA
    }else{
      string1<- sigma[1:(cut-1)]
      string2<- sigma[cut:(2*N)]
    }
    nonterminals<- draw_nt(alpha1,nonterminals_vec,N)
  }else if(grammar== "g1"){
    ss<- sample(terminals,1)
    string1<- c(ss,1)
    string2<- c(ss,2)
    nonterminals<- 2
  }else if(grammar=="g2"){
    string1<- c("a",1,"b")
    string2<- c("c",2,"d")
    nonterminals<- 2
  }else if(grammar== "g3"){
    N<- 1
    string1<- c("a",1)
    string2<- c(2,"b")
    nonterminals<- draw_nt(alpha1,nonterminals_vec,N)
  }else if(grammar== "alpha^2"){
    N<- 1
    ts<- sample(terminals,1)
    string1<- c(ts,1)
    string2<- c(ts,2)
    nonterminals<- draw_nt(alpha1,nonterminals_vec,N)
  }else if(grammar== "cf"){
    N<- 2
    string1<- 1:2
    string2<- 3:4
    nonterminals<- draw_nt(alpha1,nonterminals_vec,N)
  }else if(grammar== "regular"){
    N<- 1
    string1<- sample(terminals,1)
    string2<- 1:2
    nonterminals<- draw_nt(alpha1,nonterminals_vec,N)
  }
  
  r<- list(string1,string2,nonterminals)
  return(r)
}

##Random base rule
base_rule_random<- function(gamma1,alpha1,nonterminals_vec,terminals,grammar,p){
  type<- sample(c("emission","production"),1,prob=c(p,1-p))
  if(type=="emission"){
    if(grammar=="g0"){
      string1<- sample(terminals,1)
      string2<- sample(terminals,1)
      nonterminals<- 0
    }else if(grammar== "g1"){
      ss<- sample(terminals,1)
      string1<- ss
      string2<- ss
      nonterminals<- 0
    }else if(grammar=="g2"){
      string1<- ""
      string2<- ""
      nonterminals<- 0
    }else if(grammar=="g3"){
      string1<- "a"
      string2<- "b"
      nonterminals<- 0
    }else if(grammar== "alpha^2"){
      terminal_symbol<- sample(terminals,1)
      string1<- terminal_symbol
      string2<- terminal_symbol
      nonterminals<- 0
    }else if(grammar=="cf"){
      string1<- sample(terminals,1)
      string2<- ""
      nonterminals<- 0
    }else if(grammar=="regular"){
      string1<- sample(terminals,1)
      string2<- ""
      nonterminals<- 0
    }
    r<- list(string1,string2,nonterminals)
  }else if(type=="production"){
    r<- base_production_random(gamma1,alpha1,nonterminals_vec,terminals,grammar)
  }
  return(r)
}


##Random DP rule
dp_rule_random<- function(gamma1,alpha1,nonterminals_vec1,terminals,grammar,p,alpha2,nonterminal,nonterminals_vec,nonterminals_list,string1_list,string2_list){
  nonterminals_vec_short<- nonterminals_vec[1:length(nonterminals_list)]
  index<- which(nonterminals_vec_short== nonterminal)
  L<- length(index)
  draw<- sample(c("new","old"),1, p=c(alpha2,L))
  if(draw=="new"){
    r<- base_rule_random(gamma1,alpha1,nonterminals_vec1,terminals,grammar,p)
  }else if(draw=="old"){
    j<- sample(index,1)
    ss1<- string1_list[[j]]
    ss2<- string2_list[[j]]
    nt<- nonterminals_list[[j]]
    r<- list(ss1,ss2,nt)
  }
  return(r)
}

evaluate2<- function(s,x,j){
  w<- which(s==j)
  s<- append(s,x,after=w)
  s<- s[-w]
  return(s)
}

update_minmax1<- function(rows,mat){
  l<- length(rows)
  matrix1<- mat
  if(l>1){
    for(rrr in l:2){
      row0<- rows[rrr]
      ind0<- which(mat[,1]==mat[row0,1])
      ind1<- ind0[which(ind0!=row0)]
      row1<- rows[rrr-1]
      if(length(ind1)>0){
        matrix1[row1,5]<- min(max(matrix1[row1,5], sum(matrix1[ind0,5])), matrix1[row1,6])
        matrix1[row1,6]<- max(min(matrix1[row1,6], sum(matrix1[ind0,6])), matrix1[row1,5])
        for(i in 1:length(ind1)){
          ii<- ind1[i]
          min1<- matrix1[row1,5] + matrix1[ii,6] - sum(matrix1[ind0,6])
          max1<- matrix1[row1,6] + matrix1[ii,5] - sum(matrix1[ind0,5])
          matrix1[ii,5]<- min(max(min1,matrix1[ii,5]), matrix1[ii,6])
          matrix1[ii,6]<- max(min(max1,matrix1[ii,6]), matrix1[ii,5])
        }
        matrix1[row1,5]<- max(matrix1[row1,5], sum(matrix1[ind0,5]))
        matrix1[row1,6]<- min(matrix1[row1,6], sum(matrix1[ind0,6]))
      }else if(length(ind1)==0){
        matrix1[row0,5]<- max(matrix1[row0,5],matrix1[row1,5])
        matrix1[row1,5]<- max(matrix1[row0,5],matrix1[row1,5])
        matrix1[row0,6]<- min(matrix1[row0,6],matrix1[row1,6])
        matrix1[row1,6]<- min(matrix1[row0,6],matrix1[row1,6])
      }
    }
  }
  return(matrix1)
}

update_minmax2<- function(rows,mat){
  l<- length(rows)
  matrix2<- mat
  row1<- rows[l]
  ind0<- which(matrix2[,1]==matrix2[row1,2])
  if(length(ind0)>0){
        for(i in 1:length(ind0)){
          ii<- ind0[i]
          min1<- matrix2[row1,5] + matrix2[ii,6] - sum(matrix2[ind0,6])
          max1<- matrix2[row1,6] + matrix2[ii,5] - sum(matrix2[ind0,5])
          matrix2[ii,5]<- min(max(min1,matrix2[ii,5]), matrix2[ii,6])
          matrix2[ii,6]<- max(min(max1,matrix2[ii,6]), matrix2[ii,5])
        }
  }
  return(matrix2)
}



