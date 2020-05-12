newprod.calc_gamer<-function(p_amt,p_description){
  library(tm)
  all=length(p_description)
  #remove whitespaces if more than one, punctuations, @ and backslashes
  p_description=gsub("/"," ",p_description)
  p_description=gsub("@"," ",p_description)
  p_description=removePunctuation(p_description)
  p_description=tolower(stripWhitespace(p_description))
  p_description=gsub("\\W*\\b\\w\\b\\W*", " ", p_description)
  desc_game<-which(grepl("admiral|lotterien|leovegas|game|gaming|worldpay|skrill|digimedia|tipico|bwin|poker|lotto|wetten|gewinn|kalixa|adyen|cayden|ppro financial|betathome|bet365|bethard|betpoint|unibet|netxbetting|dumarca|betway|netbet|casino|betkick|trinity|netxbetting|safecharge|interactive ltd|rabbit|aspire global international|rootz|rabbit|william hill|mobilebet|betwinner|lotterie|prokopp|betting|unibet|betfair|winwin|winamax|\\bnwin|kicktipp|\\btipping|tippgott|tippwest ",p_description)& !grepl("gamestop|badgamenbau",p_description))
  game_amt<-p_amt[desc_game]
  game_cnt=length(game_amt)
  game_amtpos=sum(game_amt[which(game_amt>0)])/12
  game_amtneg=sum(game_amt[which(game_amt<0)])/12
  res=data.frame(game_cnt=game_cnt,all_cnt=all,game_amtpos,game_amtneg,game_rt=game_cnt/all)
  return(res)
  
}