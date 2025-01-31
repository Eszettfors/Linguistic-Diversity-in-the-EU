library(tidyverse)
library(readxl)

# import and clean data from 2024 -----------


l1_24 = read_excel("raw_data/eu_lang_data_2024.xlsx", sheet = "L1")
l2_24 = read_excel("raw_data/eu_lang_data_2024.xlsx", sheet = "L2")


## data for only l1 analysis ---------

# rough cleanup
l1_24 = l1_24 %>% select(-c(`D-W`, `D-E`)) #remove east and west germany
l1_24 = l1_24[-c(1,2, 93:98),-c(2)] # remove eu_sum, total, and "do not know" or "has no mother tongue" answers
colnames(l1_24)[1] = c("language") #change first col name

countries_24 = colnames(l1_24 %>% select(!language)) #gather countries acronymes
df_countries_24 = data.frame(countries_24)

l1_24_eng = l1_24[1:nrow(l1_24) %% 2 == 0,] #Select rows with english language names
language_names = l1_24_eng$language #store language names

l1_24_n = l1_24[1:nrow(l1_24) %% 2 == 1,] #select rows with nominal values
l1_24_n$language = language_names # add language column

l1_24_n[,2:ncol(l1_24_n)] = mapply(as.numeric ,l1_24_n %>% select(BE:SE)) # make numeric
l1_24_n[,2:ncol(l1_24_n)][is.na(l1_24_n[,2:ncol(l1_24_n)])] = 0 #replace NA with 0

fraction = function(vec){ #define function to get fraction(p)
  vec = vec/sum(vec)
  return(vec)
}

l1_24_p = l1_24_n %>% select(!language) %>% apply(MARGIN = 2, FUN = fraction) # calculate fractions
l1_24_p = data.frame(l1_24_n$language, l1_24_p) # add language column
colnames(l1_24_p)[1] = "language"
l1_24_p

sums = apply(l1_24_p %>% select(!language), MARGIN = 2, FUN =sum ) #sanity check: sum of all fractions for each country is 1

write_csv(l1_24_p, "clean_data/l1_24_p.csv")


## data for analysis with l2 - counting everyone who claimed to speak an l2 as a speaker twice -------

#Same data wrangling as previously, but adding l1 and l2 together before turning them into a fraction

l2_24 = l2_24 %>% select(-c(`D-W`, `D-E`)) #remove east and west germany
l2_24 = l2_24[-c(1,2, 93:98),-c(2)] # remove eu_sum, total, and "do not know" or "has no mother tongue" answers
colnames(l2_24)[1] = c("language") #change first col name

l2_24_eng = l2_24[1:nrow(l2_24) %% 2 == 0,] #Select rows with english language names
language_names = l2_24_eng$language #store language names

l2_24_n = l2_24[1:nrow(l2_24) %% 2 == 1,] #select rows with nominal values
l2_24_n$language = language_names # add language column

l2_24_n[,2:ncol(l2_24_n)] = mapply(as.numeric ,l2_24_n %>% select(BE:SE)) # make numeric
l2_24_n[,2:ncol(l2_24_n)][is.na(l2_24_n[,2:ncol(l2_24_n)])] = 0 #replace NA with 0

#combine number of speakers of l1 and l2
l1_l2_24_n = l1_24_n %>% select(!language) + l2_24_n %>% select(!language)


l1_l2_24_p = l1_l2_24_n %>% apply(MARGIN = 2, FUN = fraction) # calculate fractions
l1_l2_24_p = as.data.frame(l1_l2_24_p)
l1_l2_24_p$language = language_names # add language column

sums = apply(l1_l2_24_p %>% select(!language), MARGIN = 2, FUN =sum ) #sanity check: sum of all fractions for each country is 1
print(sums)

write_csv(l1_l2_24_p, "clean_data/l1_l2_24_p.csv")



# import and clean data from 2012 the same way as 2024 ---------

l1_12 = read_excel("raw_data/eu_lang_data_2012.xlsx", sheet = "L1")
l2_12 = read_excel("raw_data/eu_lang_data_2012.xlsx", sheet = "L2")

## data for only l1 analysis ---------

# rough cleanup
l1_12 = l1_12 %>% select(-c(`D-W`, `D-E`)) #remove east and west germany
l1_12 = l1_12[-c(1, 78:79),-c(2)] # remove eu_sum, total, and "other"
colnames(l1_12)[1] = c("language") #change first col name

countries_12 = colnames(l1_24 %>% select(!language)) #gather countries acronymes

l1_12_eng = l1_12[1:nrow(l1_12) %% 2 == 0,] #Select rows with english language names
language_names = l1_12_eng$language #store language names

l1_12_n = l1_12[1:nrow(l1_12) %% 2 == 1,] #select rows with nominal values
l1_12_n$language = language_names # add language column

l1_12_n[,2:ncol(l1_12_n)] = mapply(as.numeric ,l1_12_n %>% select(BE:UK)) # make numeric
l1_12_n[,2:ncol(l1_12_n)][is.na(l1_12_n[,2:ncol(l1_12_n)])] = 0 #replace NA with 0

l1_12_p = l1_12_n %>% select(!language) %>% apply(MARGIN = 2, FUN = fraction) # calculate fractions
l1_12_p = data.frame(l1_12_n$language, l1_12_p) # add language column
colnames(l1_12_p)[1] = "language"


sums = apply(l1_12_p %>% select(!language), MARGIN = 2, FUN =sum ) #sanity check: sum of all fractions for each country is 1
print(sums)

l1_12_p = l1_12_p %>% mutate(language = ifelse(language == "Irish\\ Gaelic", "Irish/Gaelic", language)) #adjusting lang name for coherence


write_csv(l1_12_p, "clean_data/l1_12_p.csv")


## data for analysis with l2 - counting everyone who claimed to speak an l2 as a speaker twice -------

#Same data wrangling as previously, but adding l1 and l2 together before turning them into a fraction

l2_12 = l2_12 %>% select(-c(`D-W`, `D-E`)) #remove east and west germany
l2_12 = l2_12[-c(1, 78:81),-c(2)] # remove eu_sum, total, and "do not know" or "has no mother tongue" answers
colnames(l2_12)[1] = c("language") #change first col name

l2_12_eng = l2_12[1:nrow(l2_12) %% 2 == 0,] #Select rows with english language names

l2_12_n = l2_12[1:nrow(l2_12) %% 2 == 1,] #select rows with nominal values

l2_12_n[,2:ncol(l2_12_n)] = mapply(as.numeric ,l2_12_n %>% select(BE:UK)) # make numeric
l2_12_n[,2:ncol(l2_12_n)][is.na(l2_12_n[,2:ncol(l2_12_n)])] = 0 #replace NA with 0

#combine number of speakers of l1 and l2
l1_l2_12_n = l1_12_n %>% select(!language) + l2_12_n %>% select(!language)


l1_l2_12_p = l1_l2_12_n %>% apply(MARGIN = 2, FUN = fraction) # calculate fractions
l1_l2_12_p = as.data.frame(l1_l2_12_p)
l1_l2_12_p$language = language_names # add language column

sums = apply(l1_l2_12_p %>% select(!language), MARGIN = 2, FUN =sum ) #sanity check: sum of all fractions for each country is 1
print(sums)

l1_l2_12_p = l1_l2_12_p %>% mutate(language = ifelse(language == "Irish\\ Gaelic", "Irish/Gaelic", language)) #adjusting lang name for coherence


write_csv(l1_l2_12_p, "clean_data/l1_l2_12_p.csv")

