library(tidyverse)
library(readxl)
library(devtools)

Picks_2020 = readxl::read_excel("2020 Picks.xlsx")
Picks_2020 = Picks_2020[1:136, 1:13] #Remove empty rows

Picks_2020$Winner = "" #Create empty columns
Picks_2020$Home_or_Away_Winner = ""
Picks_2020$'100_Dollar_Wager_Result' = 0
Picks_2020$'Favorite_win?' = ""
Picks_2020$'Favorite' = ""
Picks_2020$'Upset' = ""
Picks_2020$'Correctly_Picked_Upset' = ""

Stake = 100

#Populate Winner column
for (i in 1:nrow(Picks_2020)){
    if (Picks_2020[i, "Correct?"] == "Yes"){
      Picks_2020$Winner[i] = Picks_2020[i, "Proj_Winner"]
    }
    else if(Picks_2020[i, "Proj_Winner"] == Picks_2020[i, "Home_Team"]){
      Picks_2020$Winner[i] = Picks_2020[i, "Away_Team"]
    }
    else{
      Picks_2020$Winner[i] = Picks_2020[i, "Home_Team"]
    }
    if(Picks_2020$Winner[i] == Picks_2020$Away_Team[i]){
      Picks_2020$Home_or_Away_Winner[i] = "Away"
    }else {
      Picks_2020$Home_or_Away_Winner[i] = "Home"
    }
}

#$100 bet would result in 
for (i in 1:nrow(Picks_2020)){
  if (Picks_2020[i, "Winner"] == Picks_2020[i, "Proj_Winner"]){
    if (Picks_2020[i, "Home_or_Away_Winner"] == "Away"){ #Away
      Odds = Picks_2020[i, "Away ML"]
      if(Odds > 0){#Positive
        Picks_2020$'100_Dollar_Wager_Result'[i] = Stake * (Odds/100)
      }else{#Negative
        Picks_2020$'100_Dollar_Wager_Result'[i] = abs(Stake / (Odds/100))
      }
    }
    else if (Picks_2020[i, "Home_or_Away_Winner"] == "Home"){ #Home
      Odds = Picks_2020[i, "Home ML"]
      if(Odds > 0){#Positive
        Picks_2020$'100_Dollar_Wager_Result'[i] = Stake * (Odds/100)
      }else{#Negative
        Picks_2020$'100_Dollar_Wager_Result'[i] = abs(Stake / (Odds/100))
      }
    }
  }else{
    Picks_2020$'100_Dollar_Wager_Result'[i] = -100
  }
}

Picks_2020$`100_Dollar_Wager_Result` <- unlist(Picks_2020$`100_Dollar_Wager_Result`)
Picks_2020$'Winner' <- unlist(Picks_2020$'Winner')
Picks_2020$'Favorite' <- unlist(Picks_2020$'Favorite')
Picks_2020$'Upset' <- unlist(Picks_2020$'Upset')
Picks_2020$'Correctly_Picked_Upset' <- unlist(Picks_2020$'Correctly_Picked_Upset')

#Did the favorite win?
for (i in 1:nrow(Picks_2020)){
    if (Picks_2020[i, "Away ML"] < Picks_2020[i, "Home ML"]){
      Picks_2020$'Favorite'[i] = Picks_2020[i, "Away_Team"]
    }
    else if (Picks_2020[i, "Home ML"] < Picks_2020[i, "Away ML"]){ #Home
      Picks_2020$'Favorite'[i] = Picks_2020[i, "Home_Team"]
    }
    if(Picks_2020[i, "Winner"] == Picks_2020[i, "Favorite"]){
      Picks_2020$'Favorite_win?'[i] = "Yes"
    }else{
      Picks_2020$'Favorite_win?'[i] = "No"
    }
}
Picks_2020$'Favorite' <- unlist(Picks_2020$'Favorite')


#Correctly Pick an upset?
for (i in 1:nrow(Picks_2020)){
  Picks_2020$'Upset'[i] <- ifelse(Picks_2020$'Favorite_win?'[i] == "No", "Yes", "No")
}
  
for (i in 1:nrow(Picks_2020)){
  if(Picks_2020$'Correct?'[i] == "Yes" && Picks_2020$'Upset'[i] == "Yes"){
  Picks_2020$'Correctly_Picked_Upset'[i] = "Yes"
  }
  else if (Picks_2020$'Upset'[i] == "No"){
    Picks_2020$'Correctly_Picked_Upset'[i] = NA
  }else{
    Picks_2020$'Correctly_Picked_Upset'[i] = "No"
  }
}

write.csv(Picks_2020, "Picks_2020.csv", row.names = TRUE)





  