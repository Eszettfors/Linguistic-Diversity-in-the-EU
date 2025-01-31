library(tidyverse)
library(xtable)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(DescTools)
library(corrplot)
library(psych)


l1_24 = read.csv("clean_data/l1_24_p.csv")
l1_12 = read.csv("clean_data/l1_12_p.csv")
l1_l2_24 = read.csv("clean_data/l1_l2_24_p.csv")
l1_l2_12 = read.csv("clean_data/l1_l2_12_p.csv")


# languages in the surveys-------
l1_24$language
l1_12$language
l1_l2_24$language
l1_l2_12$language


lang_tab = data.frame(union(l1_24$language, l1_12$language))
colnames(lang_tab) = c("language")


#create a table to compare languages present in surveys

lang_tab = lang_tab %>% mutate(
  In_2024_Survey = case_when(language %in% l1_24$language ~ "yes",
                             TRUE ~ "-"),
  In_2012_Survey = case_when(language %in% l1_12$language ~ "yes",
                             TRUE ~ "-")
)

table = xtable(lang_tab)
print(table)


# Genetic distance -----

gen_dist = read_csv("clean_data/genetic_distances.csv")
gen_dist = as.data.frame(gen_dist[, -1])
langs = colnames(gen_dist)
rownames(gen_dist) = langs
dist_matrix = dist(gen_dist)

hc = hclust(dist_matrix, method = "complete")
plot(hc, labels = colnames(gen_dist), main = "Dendrogram of Genetic Distances", xlab = "", yaxt = "n", ylab = "", xaxt = "n")

gen_sim = 1-gen_dist

### square transform -----

exp_gen_dist = exp(gen_dist)
exp_dist_matrix = dist(exp_gen_dist)
hc = hclust(exp_dist_matrix, method = "complete")
plot(hc)

# Diversity -----

langs_24 = l1_24$language 
langs_12 = l1_12$language

countries_24 = colnames(l1_24[-1])
countries_12 = colnames(l1_12[-1])

sim_mat_2024 = gen_sim[colnames(gen_sim) %in% langs_24, colnames(gen_sim) %in% langs_24] # create a matrix with only the languages in the 2024 survey
sim_mat_2012 = gen_sim[colnames(gen_sim) %in% langs_12, colnames(gen_sim) %in% langs_12] # dito

lang_order = rownames(sim_mat_2012) 
l1_12 = l1_12[match(lang_order, l1_12$language),] #sort language data frame to allow for matrix mulitplication
l1_l2_12 = l1_l2_12[match(lang_order, l1_l2_12$language),]

get_A_index = function(country, df){
  # takes a country and a dataframe of which the language is a column with fraction of speakers as records
  # the A index is calculated based on the fractions in the column of that language
  vals = df[,country]
  diversity = sum(vals^2)
  return(1 - diversity)
}


get_B_index = function(country, df, similarity_matrix){
  # takes a country and a dataframe of which the language is a column with fraction of speakers as records with languages as rows
  # which are present in a supplied similarity matrix
  # the B index is calculated based on the fractions and the values for the fractions in the distance matrix
  # the function requires the languages in the dataframe to be the same as the one of the similarity matrix.
  vec = as.vector(df[, country]) #gets fractions as vector
  mat = matrix(vec) # turns it into a matrix
  mat_t = t(mat) # gets transposed matrix
  mult_mat = mat %*% mat_t # squares the one dimensional matrix
  mult_mat_w = mult_mat * similarity_matrix
  diversity = sum(mult_mat_w)
  return(1 - diversity)
}

get_richness = function(country, df){
  country_vec = df[,country]
  langs = country_vec[country_vec != 0]
  richness = length(langs)
  return(richness)
}

l1_24[, "BE"][l1_24[,"BE"] != 0]

#### test-----
get_A_index("BE", l1_24)
get_B_index("BE", l1_24, sim_mat_2024)


get_A_index("ES", l1_24)
get_B_index("ES", l1_24, sim_mat_2024)

get_richness("BE", l1_24)


get_A_index("BE", l1_12)
get_A_index("BE", l1_l2_12)


#### implementation -----

df_24 = data.frame(countries_24) 
colnames(df_24) = "country"

df_24 = df_24 %>%
          rowwise %>%
          mutate(A_index_mono_24 = get_A_index(country, l1_24),
                 B_index_mono_24 = get_B_index(country, l1_24, sim_mat_2024),
                 A_index_bi_24 = get_A_index(country, l1_l2_24),
                 B_index_bi_24 = get_B_index(country, l1_l2_24, sim_mat_2024))


df_12 = data.frame(countries_12) 
colnames(df_12) = "country"

df_12 = df_12 %>%
  rowwise %>%
  mutate(A_index_mono_12 = get_A_index(country, l1_12),
         B_index_mono_12 = get_B_index(country, l1_12, sim_mat_2012),
         A_index_bi_12 = get_A_index(country, l1_l2_12),
         B_index_bi_12 = get_B_index(country, l1_l2_12, sim_mat_2012))


df = df_24 %>% full_join(df_12)

# analysis ------

#maps data
country_names = c("Belgium", "Bulgaria",   "Czechia", 
  "Denmark","Germany", "Estonia","Ireland", "Greece", "Spain",  "France", "Croatia",  
  "Italy", "Cyprus", "Latvia", "Lithuania", "Luxembourg", "Hungary",
  "Malta", "Netherlands", "Austria","Poland", "Portugal", "Romania",  "Slovenia",
  "Slovakia", "Finland", "Sweden", "United Kingdom")
df$country_name = country_names
world = ne_countries(scale = "medium", returnclass = "sf")
eu_map = world[world$name %in% df$country_name,]
df = merge(eu_map[, c("name", "geometry")], df, by.x = "name", by.y = "country_name")


### 2024----

### pivot long
df_24_long = df %>% pivot_longer(cols = contains("24"),
                    names_to = "variable",
                    values_to = "value")

### bar charts
ggplot(data = df_24_long, aes(y = value, x = reorder(country, -value), fill = variable)) +
  geom_bar(stat = 'identity', position = "dodge") + 
  facet_wrap(~variable,
             labeller = labeller(variable = c("A_index_bi_24" = "Bilingual A Index 2024", 
                                           "A_index_mono_24" = "Monolingual A Index 2024",
                                           "B_index_bi_24" = "Bilingual B Index 2024",
                                           "B_index_mono_24" = "Monolingual B Index 2024")))+
  theme_light() + 
  theme(legend.position = "none")

### maps
ggplot(data = df_24_long) +
  geom_sf(aes(fill = value)) +
  coord_sf(xlim = c(-20,  40), ylim = c(30, 73), expand = FALSE) +
  facet_wrap(~variable,
             labeller = labeller(variable = c("A_index_bi_24" = "Bilingual A Index 2024", 
                                                       "A_index_mono_24" = "Monolingual A Index 2024",
                                                       "B_index_bi_24" = "Bilingual B Index 2024",
                                                       "B_index_mono_24" = "Monolingual B Index 2024"))) + 
  scale_fill_gradient2(low = "grey", high = "#3366FF") + 
  theme_light()

### boxplots
ggplot(data = df_24_long, aes(y = value, x = variable, fill = variable)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c("Bilingual A index 2024",
               "Monolingual A index 2024",
               "Bilingual B index 2024",
               "Monolingual B index 2024")) +
  theme(legend.position = "none") + 
  theme_light()

### Descriptive stats table
df_24_long = as_tibble(df_24_long)
desc_tab_24 = df_24_long %>% select(variable, value) %>% 
  filter(is.na(value) == FALSE) %>%
  group_by(variable) %>%
    summarise(mean = Mean(value, na.rm = "TRUE"),
              stdev = sd(value, na.rm = "TRUE"))

xtab = xtable(desc_tab_24)
print(xtab)

### corrplots
cm = cor(df_24 %>% select(-country), method = "spearman")
colnames(cm) = c("Monolingual A index 2024",
                 "Monolingual B index 2024",
                 "Bilingual A index 2024",
                 "Bilingual B index 2024")
rownames(cm) = c("Monolingual A index 2024",
                 "Monolingual B index 2024",
                 "Bilingual A index 2024",
                 "Bilingual B index 2024")
corrplot(cm, method = "number", tl.col = "black", cl.pos = "n",
         type = "lower", p.mat = pval)

psych::corr.test(df_24 %>% select(-country), method = "spearman")$p



### ranking table

head(df_24)
ranked_24 = df_24 %>% arrange(desc(A_index_mono_24))

ranks = ranked_24 %>% select(-country) %>% apply(MARGIN = 2, FUN = rank)
ranked_24[,2:ncol(ranked_24)] = ranks

head(ranked_24)

ranked_24 = ranked_24 %>% mutate(B_index_mono_24 = as.integer(B_index_mono_24 - A_index_mono_24),
                     A_index_bi_24 = as.integer(A_index_bi_24 - A_index_mono_24),
                     B_index_bi_24 = as.integer(B_index_bi_24 - A_index_mono_24),
                     A_index_mono_24 = as.integer(A_index_mono_24))

xtab = xtable(ranked_24)
print(xtab)

sum(abs(ranked_24$B_index_mono_24))
sum(abs(ranked_24$A_index_bi_24))
sum(abs(ranked_24$B_index_bi_24))

## 2012 -----


### pivot for plotting
df_12_long = df %>% pivot_longer(cols = contains("12"),
                                 names_to = "variable",
                                 values_to = "value")

### barchart
ggplot(data = as_tibble(df_12_long), aes(y = value, x = reorder(country, -value), fill = variable)) +
  geom_bar(stat = 'identity', position = "dodge") + 
  facet_wrap(~variable,
             labeller = labeller(variable = c("A_index_bi_12" = "Bilingual A Index 2012", 
                                              "A_index_mono_12" = "Monolingual A Index 2012",
                                              "B_index_bi_12" = "Bilingual B Index 2012",
                                              "B_index_mono_12" = "Monolingual B Index 2012")))+
  theme_light() + 
  theme(legend.position = "none")

### map
ggplot(data = df_12_long) +
  geom_sf(aes(fill = value)) +
  coord_sf(xlim = c(-20,  40), ylim = c(30, 73), expand = FALSE) +
  facet_wrap(~variable,
             labeller = labeller(variable = c("A_index_bi_12" = "Bilingual A Index 2012", 
                                              "A_index_mono_12" = "Monolingual A Index 2012",
                                              "B_index_bi_12" = "Bilingual B Index 2012",
                                              "B_index_mono_12" = "Monolingual B Index 2012"))) + 
  scale_fill_gradient2(high = "#3366FF") + 
  theme_light()


### boxplot
ggplot(data = df_12_long, aes(y = value, x = variable, fill = variable)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c("Bilingual A index 2012",
                              "Monolingual A index 2012",
                              "Bilingual B index 2012",
                              "Monolingual B index 2012")) +
  theme(legend.position = "none") + 
  theme_light()

### table
desc_tab_12= as_tibble(df_12_long) %>% select(variable, value) %>% 
  filter(is.na(value) == FALSE) %>%
  group_by(variable) %>%
  summarise(mean = Mean(value, na.rm = "TRUE"),
            stdev = sd(value, na.rm = "TRUE"))

xtab = xtable(desc_tab_12)
print(xtab)


### corrplot
cm = cor(df_12 %>% select(-country), method = "spearman")
colnames(cm) = c("Monolingual A index 2012",
                 "Monolingual B index 2012",
                 "Bilingual A index 2012",
                 "Bilingual B index 2012")
rownames(cm) = c("Monolingual A index 2012",
                 "Monolingual B index 2012",
                 "Bilingual A index 2012",
                 "Bilingual B index 2012")
corrplot(cm, method = "number", tl.col = "black", cl.pos = "n",
         type = "lower")

cor.test(df_12$A_index_bi_12, df_12$B_index_mono_12, method = "spearman")

### ranking table

ranked_12 = df_12 %>% arrange(desc(A_index_mono_12))

ranks = ranked_12 %>% select(-country) %>% apply(MARGIN = 2, FUN = rank)
ranked_12[,2:ncol(ranked_12)] = ranks

head(ranked_12)

ranked_12 = ranked_12 %>% mutate(B_index_mono_12 = as.integer(B_index_mono_12 - A_index_mono_12),
                                 A_index_bi_12 = as.integer(A_index_bi_12 - A_index_mono_12),
                                 B_index_bi_12 = as.integer(B_index_bi_12 - A_index_mono_12),
                                 A_index_mono_12 = as.integer(A_index_mono_12))

xtab = xtable(ranked_12)
print(xtab)

sum(abs(ranked_12$B_index_mono_12))
sum(abs(ranked_12$A_index_bi_12))
sum(abs(ranked_12$B_index_bi_12))



## 2012 vs 2024 ----


### mean table
# with croatia and UK
mean_12 = df_12 %>% select(-country) %>% apply(FUN = mean, MARGIN = 2)
mean_24 = df_24 %>% select(-country) %>% apply(FUN = mean, MARGIN = 2)     
                    
mean_tot = rbind(mean_12, mean_24)

xtab = xtable(mean_tot)
print(xtab)

mean_tot = as.data.frame(mean_tot)
mean_tot["mean_24", ] - mean_tot["mean_12",]

#test of significance
hist(df_24$A_index_mono_24)

# without Croatia and UK

mean_12 = df_12 %>% filter(country != "UK") %>% select(-country) %>% apply(FUN = mean, MARGIN = 2)
mean_24 = df_24 %>% filter(country != "HR") %>% select(-country) %>% apply(FUN = mean, MARGIN = 2)     

mean_tot = rbind(mean_12, mean_24)

xtab = xtable(mean_tot)
print(xtab)

mean_tot = as.data.frame(mean_tot)
mean_tot["mean_24", ] - mean_tot["mean_12",]

df_12_small = df_12 %>% filter(country != "UK")
df_24_small = df_24 %>% filter(country != "HR")

### plots

df_change = df %>% filter(! country %in% c("UK", "HR")) %>%
  mutate(change_A_mono = A_index_mono_24 - A_index_mono_12,
         change_B_mono = B_index_mono_24 - B_index_mono_12,
         change_A_bi = A_index_bi_24 - A_index_bi_12,
         change_B_bi = B_index_bi_24 - B_index_bi_12) %>%
  select(country, contains("change"), geometry)


df_change_long = df_change %>% pivot_longer(cols = contains("change"),
                                 names_to = "variable",
                                 values_to = "value")
head(df_change_long)

### barchart
ggplot(data = as_tibble(df_change_long), aes(y = value, x = reorder(country, -value), fill = variable)) +
  geom_bar(stat = 'identity', position = "dodge") + 
  facet_wrap(~variable,
             labeller = labeller(variable = c("change_A_bi" = "Bilingual A Index",
                                              "change_A_mono" = "Monolingual A Index",
                                              "change_B_bi" = "Bilingual B Index",
                                              "change_B_mono" = "Monolingual B Index"))) + 
  labs(y = "Difference in Diversity Index") +
  theme_light() + 
  theme(legend.position = "none")


### map
ggplot(data = df_change_long) +
  geom_sf(aes(fill = value)) +
  coord_sf(xlim = c(-20,  40), ylim = c(30, 73), expand = FALSE) +
  facet_wrap(~variable,
             labeller = labeller(variable = c("change_A_bi" = "Bilingual A Index",
                                              "change_A_mono" = "Monolingual A Index",
                                              "change_B_bi" = "Bilingual B Index",
                                              "change_B_mono" = "Monolingual B Index")))  + 
  scale_fill_gradient2(low = "red", high = "#3366FF", midpoint = 0) +
  theme_light() 


