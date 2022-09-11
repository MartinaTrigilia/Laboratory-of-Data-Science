library(tidyverse)

#### Lettura dati ####

dati <- read.csv("C:\\Users\\giann\\Desktop\\Laurea_Magistrale\\LabDS\\progetto\\dati\\tennis_finale.csv")
for(col in colnames(dati)){
  dati[[col]][dati[[col]] == ""] <- NA
}

colSums(is.na(dati))


sapply(dati, class)

dati$match_id <- paste0(dati$tourney_id, as.character(dati$match_num))
dup <- dati[duplicated(dati$match_id),]

t <- dati[dati$tourney_id == "2019-W-ITF-GBR-01C-2019",]
mn <- t$match_num[t$winner_name == "Indy De Vroome"]
t$winner_name[t$match_num == mn[1]]
t$winner_name[t$match_num == mn[2]]
t$winner_name[t$match_num == mn[3]]
t$winner_name[t$match_num == mn[4]]
t$winner_name[t$match_num == mn[5]]
t$winner_name[t$match_num == mn[6]]

#### Guardo un po' i dati mancanti se hanno valori in altre righe

tornei <- dati %>%
  select(tourney_id, tourney_name, surface, draw_size, tourney_level, tourney_date, winner_id, winner_name, winner_entry,
         loser_id, loser_name, loser_entry)

tornei %>%
  group_by(tourney_id, winner_id) %>%
  select(tourney_id, winner_name, winner_entry)
  
nomi <- tornei$loser_name[is.na(tornei$loser_entry)]
nomi <- unique(nomi) #gicatori con almeno un NA in loser_entry
df_loser_entry <- data.frame(nome = nomi, torneo = NA)

for(nome in nomi){
  if(is.na(df_loser_entry$torneo[df_loser_entry$nome == nome])){
    df_loser_entry$torneo[df_loser_entry$nome == nome] <- list(tornei$tourney_id[is.na(tornei$loser_entry) & tornei$loser_name == nome])
  }
  else{
    append(df_loser_entry$torneo[df_loser_entry$nome == nome], tornei$tourney_id[is.na(tornei$loser_entry) & tornei$loser_name == nome])
  }
}

tornei$loser_entry[tornei$tourney_id == df_loser_entry$torneo[[1]][3] & tornei$loser_name == df_loser_entry$nome[1]]

## NOT RUN ##
#df <- data.frame(nome = nomi, sumnotna = 0)
#for(i in c(1:dim(df_loser_entry)[1])){
#  for(j in c(1,length(tornei$loser_entry[tornei$tourney_id == df_loser_entry$torneo[[1]][i] & tornei$loser_name == df_loser_entry$nome[i]]))){
#    df$sumnotna[df$nome == df_loser_entry$nome[i]] <-
#      sum(!is.na(tornei$loser_entry[tornei$tourney_id == df_loser_entry$torneo[[1]][j] & tornei$loser_name == df_loser_entry$nome[1]]))
#  }
#}

# Per ogni torneo ho guardato per ogni giocatore che aveva winner_entry o loser_entry mancante in almeno una riga, se c'erano almeno un'altra
# riga con valore non mancante e non ne ho trovate. Non so quindi come rimpiazzare queste righe.


#### Correzione IOC ####
correggere <- dati$winner_name[dati$winner_ioc == "Southsea 16 Main Draw Friday Final No Qualifying"]
dati$winner_ioc[dati$winner_name == correggere[1]] <- "ROU"
dati$winner_ioc[dati$winner_name == correggere[2]] <- "BIH"
dati$winner_ioc[dati$winner_name == correggere[3]] <- "RUS"
dati$winner_ioc[dati$winner_name == correggere[4]] <- "USA"
dati$winner_ioc[dati$winner_name == correggere[5]] <- "RUS"
dati$winner_ioc[dati$winner_name == correggere[6]] <- "JPN"
dati$winner_ioc[dati$winner_name == correggere[7]] <- "GER"
dati$winner_ioc[dati$winner_name == correggere[8]] <- "USA"

#### winner_hand (loser_hand) ####

# ci basiamo su Francois Fagan et al. 2018 (https://doi.org/10.1515/jqas-2017-0076) -> 15% dei tennisti sono mancini

##======================================================================================================##
## Mettendo gli "U" come NA ##
##======================================================================================================##

## WINNER_HAND

dati$winner_hand[dati$winner_hand == "U"] <- NA

nomi <- unique(dati$winner_id[is.na(dati$winner_hand)])

# controllo se per ogni giocatore con almeno un NA in winner_hand, ho una riga non NA
dfnomi <- data.frame(nome = nomi, hand = NA)
for(nome in nomi){
  if(is.na(dfnomi$hand[dfnomi$nome == nome])){
    dfnomi$hand[dfnomi$nome == nome] <- list(dati$winner_hand[dati$winner_id == nome])
  }
  else{
    append(dfnomi$hand[dfnomi$nome == nome], dati$winner_hand[dati$winner_id == nome])
  }
}

risul <- c(NA)
for(i in c(1:dim(dfnomi)[1])){
  risul <- c(risul, sum(!is.na(unlist(dfnomi$hand[i]))))
}
risul <- risul[2:length(risul)]
sum(risul) #ci sono alcuni giocatori da cui si può recuperare winner_hand
pid <- which(risul != 0)
x <- dfnomi[pid,]
newdf <- data.frame(id = x$nome, hand = NA)
for(i in c(1:dim(x)[1])){
  hand <- unique(unlist(x$hand[i]))
  hand <- hand[!is.na(hand)]
  newdf$hand[i] <- hand
}
colnames(newdf) <- c("winner_id", "winner_hand")
#sostituisco nei dati originali
dati <- merge(dati, newdf, by = "winner_id", all.x = TRUE)
dati <- transform(dati, winner_hand = ifelse(is.na(winner_hand.y), winner_hand.x, as.character(winner_hand.y)))
dati$winner_hand.x <- NULL
dati$winner_hand.y <- NULL #elimino le colonne superflue create da merge

for(id in nomi){
  dati$winner_hand[dati$winner_id == id] <- ifelse(rbernoulli(1, p = 0.15), "L", "R")
}
round(table(dati$winner_hand)/(sum(table(dati$winner_hand))), 3)

## LOSER_HAND

dati$loser_hand[dati$loser_hand == "U"] <- NA

nomi <- unique(dati$loser_id[is.na(dati$loser_hand)])

# controllo se per ogni giocatore con almeno un NA in loser_hand, ho una riga non NA
dfnomi <- data.frame(nome = nomi, hand = NA)
for(nome in nomi){
  if(is.na(dfnomi$hand[dfnomi$nome == nome])){
    dfnomi$hand[dfnomi$nome == nome] <- list(dati$loser_hand[dati$loser_id == nome])
  }
  else{
    append(dfnomi$hand[dfnomi$nome == nome], dati$loser_hand[dati$loser_id == nome])
  }
}

risul <- c(NA)
for(i in c(1:dim(dfnomi)[1])){
  risul <- c(risul, sum(!is.na(unlist(dfnomi$hand[i]))))
}
risul <- risul[2:length(risul)]
sum(risul) #ci sono alcuni giocatori da cui si può recuperare loser_hand
pid <- which(risul != 0)
x <- dfnomi[pid,]
newdf <- data.frame(id = x$nome, hand = NA)
for(i in c(1:dim(x)[1])){
  hand <- unique(unlist(x$hand[i]))
  hand <- hand[!is.na(hand)]
  newdf$hand[i] <- hand
}
colnames(newdf) <- c("loser_id", "loser_hand")
#sostituisco nei dati originali
dati <- merge(dati, newdf, by = "loser_id", all.x = TRUE)
dati <- transform(dati, loser_hand = ifelse(is.na(loser_hand.y), loser_hand.x, as.character(loser_hand.y)))
dati$loser_hand.x <- NULL
dati$loser_hand.y <- NULL #elimino le colonne superflue create da merge

for(id in nomi){
  dati$loser_hand[dati$loser_id == id] <- ifelse(rbernoulli(1, p = 0.15), "L", "R")
}
round(table(dati$loser_hand)/(sum(table(dati$loser_hand))), 3)


#### ALTEZZA ####
table(dati$winner_ioc[is.na(dati$winner_ht)])
table(dati$loser_ioc[is.na(dati$loser_ht)])

summary(dati$winner_ht)
sort(dati$winner_ht[!is.na(dati$winner_ht)])[1:100]

summary(dati$loser_ht)
sort(dati$loser_ht[!is.na(dati$loser_ht)])[1:100] #ci sono alcuni 2. Li metto NA

dati$winner_ht[dati$winner_ht == min(dati$winner_ht,na.rm=T)] <- NA
dati$loser_ht[dati$loser_ht == min(dati$loser_ht,na.rm=T)] <- NA

## Aggiungo il sesso

mp <- read.csv("C:\\Users\\giann\\Desktop\\Laurea_Magistrale\\LabDS\\progetto\\dati\\male_players.csv")
mp$full_name <- paste(mp$name, mp$surname, sep = " ")
mp$gender <- "M"
fp <- read.csv("C:\\Users\\giann\\Desktop\\Laurea_Magistrale\\LabDS\\progetto\\dati\\female_players.csv")
fp$full_name <- paste(fp$name, fp$surname, sep = " ")
fp$gender <- "F"

dati$gender <- NA
dati$gender[dati$winner_name %in% mp$full_name] <- "M"
dati$gender[dati$loser_name %in% mp$full_name] <- "M"

dati$gender[dati$winner_name %in% fp$full_name] <- "F"
dati$gender[dati$loser_name %in% fp$full_name] <- "F"

## Creo un data.frame dei giocatori
# loser
players <- data.frame(id = unique(c(dati$loser_id, dati$winner_id)))
df_loser <- dati[c("loser_id", "gender", "loser_ht", "loser_ioc")]
df_loser <- df_loser[!duplicated(df_loser$loser_id),]
players <- players %>% left_join(df_loser, by = c("id" = "loser_id"))

# winner
df_winner <- dati[c("winner_id", "gender", "winner_ht", "winner_ioc")]
df_winner <- df_winner[!duplicated(df_winner$winner_id),]
players <- players %>% left_join(df_winner, by = c("id" = "winner_id"))

length(players$loser_ht[!is.na(players$loser_ht) & is.na(players$winner_ht)])
length(players$winner_ht[!is.na(players$winner_ht) & is.na(players$loser_ht)])

players$gender <- coalesce(players$gender.x, players$gender.y)
players$gender.x <- NULL
players$gender.y <- NULL
players$ht <- coalesce(players$winner_ht, players$loser_ht)
players$winner_ht <- NULL
players$loser_ht <- NULL
players$ioc <- coalesce(players$winner_ioc, players$loser_ioc)
players$loser_ioc <- NULL
players$winner_ioc <- NULL
addmargins(table(is.na(players$ht), players$gender)) #183/7065 = 0.0259 F - 341/2698 = 0.1264 M

summary(players$ht)

#sostituisco ogni NA con la mediana della nazione corrispodente al codice IOC
ioc <- unique(players$ioc)

for(nazione in ioc){
  median_nazione_F <- median(players$ht[players$ioc == nazione & players$gender == "F"], na.rm = TRUE)
  median_nazione_M <- median(players$ht[players$ioc == nazione & players$gender == "M"], na.rm = TRUE)
  
  players$ht[is.na(players$ht) & players$ioc == nazione & players$gender == "F"] <-median_nazione_F
  players$ht[is.na(players$ht) & players$ioc == nazione & players$gender == "M"] <-median_nazione_M
}

summary(players$ht)

## PRE
# Min.   1st Qu.  Median  Mean  3rd Qu.    Max.    NA's 
# 155.0   175.0   183.0   181.7   188.0   211.0   9580 

## POST
# Min.   1st Qu.  Median  Mean  3rd Qu.    Max.    NA's 
# 155.0   172.0   175.5   176.9   183.0   211.0    1504

# La distribuzione si sposta verso sinistra, abbassandosi tutto da primo quartile a terzo quartile
# Alcuni NA rimangono

dati <- left_join(dati, players[c(1,3)], by = c("loser_id" = "id"))
dati <- left_join(dati, players[c(1,3)], by = c("winner_id" = "id"))

dati$loser_ht <- dati$ht.x
dati$winner_ht <- dati$ht.y
dati$ht.x <- NULL
dati$ht.y <- NULL

### Età ####
pnomi <- unique(dati$loser_name[is.na(dati$loser_age)])

sum(!is.na(dati$loser_age[dati$loser_id %in% pnomi]))


which(!is.na(dati$loser_age[dati$loser_id %in% pnomi]))
dati$loser_name[3]
dati$loser_age[3]

dfnomi <- data.frame(nome = pnomi, age = NA)
for(nome in pnomi){
  if(is.na(dfnomi$age[dfnomi$nome == nome])){
    dfnomi$age[dfnomi$nome == nome] <- list(dati$loser_age[dati$loser_name == nome])
  }
  else{
    append(dfnomi$age[dfnomi$nome == nome], dati$loser_age[dati$loser_name == nome])
  }
}

# numero di giocatori che hanno l'età mancante in almeno una riga ma non mancante in un'altra
risul <- c(NA)
for(i in c(1:dim(dfnomi)[1])){
  risul <- c(risul, sum(!is.na(unlist(dfnomi$age[i]))))
}
sum(risul, na.rm = TRUE)
# il problema dell'età è che ovviamente varia con la data del torneo, a differenza della mano usata ad esempio.
# abbiamo deciso di lasciarla così


#### Minuti ####
summary(dati$minutes)
library(e1071)
kurtosis(dati$minutes, na.rm = TRUE)
ggplot(dati, aes(x=minutes)) + geom_density() #coda destra enorme. Uso la mediana
dati$minutes[is.na(dati$minutes)] <- median(dati$minutes, na.rm = T)

#### Ace ####
summary(dati$w_ace)
summary(dati$l_ace)
x <- data.frame(name = dati$winner_name, ace = dati$w_ace, w = 1)
x2 <- data.frame(name = dati$loser_name, ace = dati$l_ace, w = 0)
x <- rbind(x,x2)
summary(aov(ace ~ w, data = x))
summary(glm(w ~ ace, data = x, family = "binomial"))
# il numero di ace sembra essere "correlato" con la vittoria. Sostituisco i mancanti con la mediana per gruppo
summary(x$ace[x$w == 1])
summary(x$ace[x$w == 0])

dati$w_ace[is.na(dati$w_ace)] <- median(dati$w_ace, na.rm = T)
dati$l_ace[is.na(dati$l_ace)] <- median(dati$l_ace, na.rm = T)

#### BP_faced ####
summary(dati$w_bpFaced)
summary(dati$l_bpFaced)

dati$w_bpFaced[is.na(dati$w_bpFaced)] <- median(dati$w_bpFaced, na.rm = T)
dati$l_bpFaced[is.na(dati$l_bpFaced)] <- median(dati$l_bpFaced, na.rm = T)

#### BP_saved ####
summary(dati$w_bpSaved)
summary(dati$l_bpSaved)

dati$w_bpSaved[is.na(dati$w_bpSaved)] <- median(dati$w_bpSaved, na.rm = T)
dati$l_bpSaved[is.na(dati$l_bpSaved)] <- median(dati$l_bpSaved, na.rm = T)

#### SVPT ####
summary(dati$w_svpt)
summary(dati$l_svpt)

dati$w_svpt[is.na(dati$w_svpt)] <- median(dati$w_svpt, na.rm = T)
dati$l_svpt[is.na(dati$l_svpt)] <- median(dati$l_svpt, na.rm = T)


#### DF ####
summary(dati$w_df)
summary(dati$l_df)

dati$w_df[is.na(dati$w_df)] <- median(dati$w_df, na.rm = T)
dati$l_df[is.na(dati$l_df)] <- median(dati$l_df, na.rm = T)


#### 1stIn ####
summary(dati$w_1stIn)
summary(dati$l_1stIn)

dati$w_1stIn[is.na(dati$w_1stIn)] <- median(dati$w_1stIn, na.rm = T)
dati$l_1stIn[is.na(dati$l_1stIn)] <- median(dati$l_1stIn, na.rm = T)

#### 1stWon ####
summary(dati$w_1stWon)
summary(dati$l_1stWon)

dati$w_1stWon[is.na(dati$w_1stWon)] <- median(dati$w_1stWon, na.rm = T)
dati$l_1stWon[is.na(dati$l_1stWon)] <- median(dati$l_1stWon, na.rm = T)


#### 2ndWon ####
summary(dati$w_2ndWon)
summary(dati$l_2ndWon)

dati$w_2ndWon[is.na(dati$w_2ndWon)] <- median(dati$w_2ndWon, na.rm = T)
dati$l_2ndWon[is.na(dati$l_2ndWon)] <- median(dati$l_2ndWon, na.rm = T)


#### SvGms ####
summary(dati$w_SvGms)
summary(dati$l_SvGms)

dati$w_SvGms[is.na(dati$w_SvGms)] <- median(dati$w_SvGms, na.rm = T)
dati$l_SvGms[is.na(dati$l_SvGms)] <- median(dati$l_SvGms, na.rm = T)


#### score ####
sum(is.na(dati$score))
dati <- dati[!is.na(dati$score),]

#### Surface ####
dfsurface <- as.data.frame.matrix(table(dati$tourney_id, dati$surface, useNA = "always"))
colnames(dfsurface) <- c("carpet", "clay" , "grass", "hard", "nNA")
dfsurface_subset <- dfsurface[dfsurface$nNA != 0,] #nessuna delle righe con surface NA ha un valore non NA in un'altra riga

torneidf <- dati %>%
  group_by(tourney_id) %>%
  select(tourney_id, tourney_name, surface)

torneidf <- torneidf[!duplicated(torneidf$tourney_id),]

probabilities <- cumsum(table(torneidf$surface)/sum(table(torneidf$surface)))
tornei <- unique(dati$tourney_id)
for(torneo in tornei){
  # genero un valore casuale da una distribuzione uniforme [0,1] e controllo in quale degli intervalli definiti in
  # "probabilities" cade e metto la superficie corrispondente
  r <- runif(1)
  dati$surface[is.na(dati$surface) & dati$tourney_id == torneo] <-
    ifelse(r <= probabilities[1], names(probabilities)[1],
          ifelse(r <= probabilities[2], names(probabilities)[2],
                ifelse(r <= probabilities[3], names(probabilities)[3],
                      names(probabilities)[4]
                      )
                )
          )
}

#### Scrittura finale ####
setwd("C:\\Users\\giann\\Desktop\\Laurea_Magistrale\\LabDS\\progetto\\dati")
write.csv(dati, file = "tennis_no_missing.csv")

#sapply(dati[,c(23:41)], median)

