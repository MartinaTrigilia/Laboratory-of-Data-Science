library(tidyverse)
library(moments)
library(ggpmisc)
library(waffle)

#### Lettura dati ####

dati <- read.csv("C:\\Users\\giann\\Desktop\\Laurea_Magistrale\\LabDS\\progetto\\dati\\tennis_finale.csv")
for(col in colnames(dati)){
  dati[[col]][dati[[col]] == ""] <- NA
}

# Correggo IOC
correggere <- dati$winner_name[dati$winner_ioc == "Southsea 16 Main Draw Friday Final No Qualifying"]
dati$winner_ioc[dati$winner_name == correggere[1]] <- "ROU"
dati$winner_ioc[dati$winner_name == correggere[2]] <- "BIH"
dati$winner_ioc[dati$winner_name == correggere[3]] <- "RUS"
dati$winner_ioc[dati$winner_name == correggere[4]] <- "USA"
dati$winner_ioc[dati$winner_name == correggere[5]] <- "RUS"
dati$winner_ioc[dati$winner_name == correggere[6]] <- "JPN"
dati$winner_ioc[dati$winner_name == correggere[7]] <- "GER"
dati$winner_ioc[dati$winner_name == correggere[8]] <- "USA"


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

#### Plot pre-sostituzione ####

theme_set(theme_bw())

distr_pre <- as.matrix(summary(players$ht))
summ <- data.frame(min = distr_pre[1], q1 = distr_pre[2], median = distr_pre[3], mean = round(distr_pre[4], 1), q3 = distr_pre[5], max = distr_pre[6], na = distr_pre[7])

pre <- players %>%
  ggplot(aes(ht, fill = as.factor(gender))) +
  geom_histogram(bins = 60, alpha = 0.5, position = "identity") +
  lims(y = c(0, 420)) +
  labs(title = "Distribuzione altezza pre-sostituzione", x = "altezza", y = "frequenza",
       fill = "Sesso") +
  geom_table_npc(data = summ, label = list(summ), npcx = 0.015, npcy = 0.95, hjust = 0, vjust = 1,
                 size = 7) + #aggiungo la summary al grafico
  theme(title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

#### Sostituzione altezza ####
#sostituisco ogni NA con la mediana della nazione corrispodente al codice IOC
ioc <- unique(players$ioc)

for(nazione in ioc){
  median_nazione_F <- median(players$ht[players$ioc == nazione & players$gender == "F"], na.rm = TRUE)
  median_nazione_M <- median(players$ht[players$ioc == nazione & players$gender == "M"], na.rm = TRUE)
  
  players$ht[is.na(players$ht) & players$ioc == nazione & players$gender == "F"] <-median_nazione_F
  players$ht[is.na(players$ht) & players$ioc == nazione & players$gender == "M"] <-median_nazione_M
}

#### Plot post-sostituzione ####

distr_post <- as.matrix(summary(players$ht))

summ <- data.frame(min = distr_post[1], q1 = distr_post[2], median = distr_post[3], mean = round(distr_post[4], 2), q3 = distr_post[5], max = distr_post[6], na = distr_post[7])

post <- players %>%
  ggplot(aes(ht, fill = as.factor(gender))) +
  geom_histogram(bins = 60, alpha = 0.5, position = "identity") +
  lims(y = c(0, 420)) +
  labs(title = "Distribuzione altezza post-sostituzione", x = "altezza", y = "frequenza",
       fill = "Sesso") +
  geom_table_npc(data = summ, label = list(summ), npcx = 0.015, npcy = 0.95, hjust = 0, vjust = 1,
                 size = 7) + #aggiungo la summary al grafico
  theme(title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

gridExtra::grid.arrange(pre, post, ncol = 2)

#### Superficie ####

tornei <- dati %>%
  group_by(tourney_id) %>%
  select(tourney_id, tourney_name, surface)

tornei <- tornei[!duplicated(tornei$tourney_id),]

parts <- round(table(tornei$surface)/sum(table(tornei$surface)), 2) * 100
parts <- sort(parts, descending = TRUE)
library(waffle)
waffle(parts, rows = 10, equal = TRUE, colors = c("yellow2", "green2", "chocolate3", "dodgerblue2"),
       title = "Distribuzione di surface tra tourney_id") +
  theme(title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.key.size = unit(1.5, "line"))




