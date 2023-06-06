n<- 2
sample(1:length(tab), n, replace= TRUE, prob = (tab + C_nonterminals))


tab<- table(nonterminals_vec_long)
w1<- tab[ind1]/(sum(tab))/((tab[ind1]+C_nonterminals)/sum(tab + C_nonterminals))
w2<- tab[ind2]/(sum(tab))/((tab[ind1]+C_nonterminals)/sum(tab + C_nonterminals))

draw_nt<- function(n){
  nt<- vector(length=n)
  ntv<-nonterminals_vec_long
  for(i in 1:n){
    if(length(ntv)==0){
      nt[i]<- 1
    }else{
      tab<- table(nonterminals_vec_long)
      draw<- sample(c("new","old"),1,prob=c(alpha1, sum(tab + C_nonterminals) ))
      if(draw=="old"){
        s<- sample(1:length(tab), n, replace= TRUE, prob = (tab + C_nonterminals))
        nt[i]<- s
      }else if(draw=="new"){
        nt[i]<- length(tab)+1
      }
    }
    ntv<- c(ntv,nt[i])
  }
  return(nt)
}