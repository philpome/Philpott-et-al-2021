setwd("/Users/meganphilpott/Desktop")
articles <- read.csv("wos-dataset-3-2023.csv",header=TRUE)
species_list <- read.csv("SpeciesAmericas_v2.csv",header=TRUE)
species_basic <- read.csv("species-no-dupes.csv",header=TRUE,encoding = "UTF-8")
genus_species <- read.csv("genus_species-no-dupes.csv",header=TRUE)
genus_only <- read.csv("genus_only.csv",header=TRUE)
countries_dict <- read.csv("countries-dict.csv",header=TRUE,encoding = "UTF-8")
test_data <- read.csv("test-wos-dataset.csv",header=TRUE)
test_data <- as_tibble(test_data)
library(stringr)
library(plyr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(tidyr)

articles <- as_tibble(articles)
species_list <- as_tibble(species_list)
species_basic <- as_tibble(species_basic)
countries_dict <- as_tibble(countries_dict)
genus_species <- as_tibble(genus_species)
pub2 <- articles %>% mutate(american_species = 1L*map_lgl(articles$Abstract, ~any(str_detect(.x, species_basic$Species))))
pub3 <- pub2 %>% mutate(american_countries = 1L*map_lgl(pub2$Abstract, ~any(str_detect(.x, countries_dict$country))))

#IGNORE THE HASHTAGGED CODE
#test data: 
#pub-test <- test_data %>% mutate(american_species = 1L*map_dbl(test_data$Abstract, ~any(str_subset(.x, species_basic$Species))))
#testtest<-map_dfc(species_basic$Species, str_subset, string = test_data$Abstract)

#test_result <- lapply(strsplit(test_data$Abstract, split=" "), function(x) { x[x %in% species_list$Species] })
#lapply(test_data$Abstract, function(x) regmatches(x, gregexpr(paste(species, collapse = "|"), x)))
#result <- lapply(test_result, `length<-`, max(lengths(test_result)))
#testtest<-cbind(test_data[ , 'Authors', drop = F], do.call(rbind, result))

#result <- t(apply(test_data, 1,
#                  function(x) str_extract_all(x[['Abstract']], genus_species$Genus, simplify = TRUE)))

#countries_dict$country<-str_to_upper(countries_dict$country)
#species_basic$Species<-str_to_upper(species_basic$Species)
#species_list$Species<-str_to_upper(species_list$Species)
#species_list$Country<-str_to_upper(species_list$Country)
#test_data$Abstract<-str_to_upper(test_data$Abstract)
#genus_species$Genus<-str_to_upper(genus_species$Genus)
#genus_species$Species<-str_to_upper(genus_species$Species)
#genus_only$Genus<-str_to_upper(genus_only$Genus)



#genus <- genus_species$Genus
#genus <- genus_only$Genus
#testtesttest<-c(test_data$Abstract)
#testtesttest<-tibble(line=1:24, text=testtesttest)
#library(tidytext)
#testtesttest %>%
#  unnest_tokens(word, text)
#focal_genera<-str_extract(testtesttest, genus)
#data("stop_words")
#testtesttest %>% count(word)

#str_extract(testtesttest, str_c(str_c(" ", genus)))
#str_extract(testtesttest, "PANAX")
#str_extract(testtesttest, genus)
#str_detect(testtesttest, genus)




#start here to test the code on a small subset dataset: 
#add an index identifier to group abstracts: 
test_data$index <- 1:nrow(test_data)
#subset the data by abstracts that include species in our species list:
pubtest2 <- test_data %>% mutate(american_species = 1L*map_dbl(test_data$Abstract, ~any(str_extract(.x, genus_species$Genus))))
#tokenize the dataset so each word in abstract occupies one row:
tidy_abstract <- pubtest2 %>%
  unnest_tokens(word, Abstract)
#remove stop words:
data("stop_words")
tidy_abstract <- tidy_abstract %>%
  anti_join(stop_words)
#what's the most common word in the abstracts?
tidy_abstract %>%
  count(word, sort = TRUE) 
tidy_abstract %>%
  count(word, sort = TRUE) %>%
  filter(n > 25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
#genetic, conservation, and populations - that bodes well
#let's do a word cloud for fun:
tidy_abstract %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
#let's make a dataset with bigrams:
abstract_bigrams <- pubtest2 %>%
  unnest_tokens(bigram, Abstract, token = "ngrams", n=2) %>%
  filter(!is.na(bigram))
abstract_bigrams %>%
  count(bigram, sort=TRUE)
#filter out the stop words: 
bigrams_separated <- abstract_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts
#reunite the bigrams without stop words: 
abstract_bigrams_filtered <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ")
#find the overlap with our species list:
species_bigrams <- species_basic %>%
  unnest_tokens(bigram, Species, token = "ngrams", n=2) %>%
  filter(!is.na(bigram))
filtered_abstract_test <- inner_join(abstract_bigrams_filtered, species_bigrams)

#now for real using the full dataset: 
#add an index identifier to group abstracts: 
articles$index <- 1:nrow(articles)
write.csv(articles, "articles-with-index.csv")
#subset the data by abstracts that include species in our species list:
pub2 <- articles %>% mutate(american_species = 1L*map_lgl(articles$Abstract, ~any(str_detect(.x, species_basic$Species))))
#use the code below only if we want to pull countries out of the abstracts as well
#I did this first but it makes the species-author match difficult.
#pub3 <- pub2 %>% mutate(american_countries = 1L*map_lgl(pub2$Abstract, ~any(str_detect(.x, countries_dict$country))))
#tokenize the dataset so each word in abstract occupies one row:
tidy_abstracts <- pub2 %>%
  unnest_tokens(word, Abstract)
#remove stop words:
data("stop_words")
tidy_abstracts <- tidy_abstracts %>%
  anti_join(stop_words)
#what's the most common word in the abstracts?
tidy_abstracts %>%
  count(word, sort = TRUE) 
#words appearing more than 5,000 times in the dataset:
tidy_abstracts %>%
  count(word, sort = TRUE) %>%
  filter(n > 5000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL, title = "Words appearing >5,000x")
ggsave("word-freq.jpg",dpi=300)
#genetic, species, and populations - that bodes well
#let's do a word cloud for fun:
tidy_abstracts %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
#species, conservation, and populations were too frequent and aren't shown
ggsave("word-cloud.jpg",dpi=300)
#let's convert the abstracts to bigrams (2-word strings):
abstract_bigrams <- pub2 %>%
  unnest_tokens(bigram, Abstract, token = "ngrams", n=2) %>%
  filter(!is.na(bigram))
abstract_bigrams %>%
  count(bigram, sort=TRUE)
#need to filter out the stop words (of, a, the, etc.): 
bigrams_separated <- abstract_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts
#reunite the bigrams without stop words: 
abstract_bigrams_filtered <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ")
#find the overlap with our species list:
species_bigrams <- species_basic %>%
  unnest_tokens(bigram, Species, token = "ngrams", n=2) %>%
  filter(!is.na(bigram))
filtered_abstract_by_species <- inner_join(abstract_bigrams_filtered, species_bigrams)
write.csv(filtered_abstract_by_species, "filtered-species2.csv")

#now we'll filter for country:
#first bigrams:
country_bigrams <- countries_dict %>%
  unnest_tokens(bigram, country, token = "ngrams", n=2) %>%
  filter(!is.na(bigram))
country_bigrams %>%
  count(bigram, sort=TRUE)
country_bigrams_separated <- country_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
country_bigrams_filtered <- country_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
country_bigram_counts <- country_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
country_bigram_counts
#reunite the bigrams without stop words: 
country_bigrams_filtered <- country_bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ")
#add in single-word countries:
countries_dict <- countries_dict %>%
  unnest_tokens(word, country) %>%
  anti_join(stop_words)
country_dict_formatted <- bind_rows(country_bigrams_filtered %>%
                                      unnest_tokens(country_match, bigram,
                                                    token="ngrams", n=2), 
                                    countries_dict %>%
                                      unnest_tokens(country_match, word))
#add in single words from abstracts: 
tidy_abstracts_single_bigram <- bind_rows(abstract_bigrams_filtered %>%
                                            unnest_tokens(country_match, bigram,
                                                          token="ngrams",n=2),
                                          tidy_abstracts %>%
                                            unnest_tokens(country_match, word))
#find the overlap with our country list:
filtered_abstract_by_country <- inner_join(tidy_abstracts_single_bigram, country_dict_formatted)
write.csv(filtered_abstract_by_country, "filtered-country.csv")

#create a dataset that filters by both country and/or species: 
tidy_abstracts_all <- bind_rows(abstract_bigrams_filtered %>%
                                            unnest_tokens(country_species_match, bigram,
                                                          token="ngrams",n=2),
                                          tidy_abstracts %>%
                                            unnest_tokens(country_species_match, word))

country_dict_all <- bind_rows(country_bigrams_filtered %>%
                                      unnest_tokens(country_species_match, bigram,
                                                    token="ngrams", n=2), 
                                    countries_dict %>%
                                      unnest_tokens(country_species_match, word))

species_dict_all <- species_basic %>%
  unnest_tokens(country_species_match, Species, token = "ngrams", n=2) %>%
  filter(!is.na(country_species_match))

country_species_dict <- rbind(country_dict_all, species_dict_all)
pub_bias_filtered <- inner_join(tidy_abstracts_all, country_species_dict)
#Okay, I'm adding a second dataset of abstracts just filtered by species
#the below dataset does not include countries mentioned in abstracts!!
pub_bias_filtered2 <- filtered_abstract_by_species
write.csv(pub_bias_filtered, "pub-bias-filtered.csv")
#remove duplicates 
pub_bias_filtered_no_dupes <- pub_bias_filtered[!duplicated(pub_bias_filtered[c("index","country_species_match")]),]
#or with just species:
pub_bias_filtered_no_dupes2 <- pub_bias_filtered2[!duplicated(pub_bias_filtered2[c("index","bigram")]),]
write.csv(pub_bias_filtered_no_dupes, "pub-bias-filtered-no-dupes.csv")
length(unique(pub_bias_filtered_no_dupes2$index))
#leaves us a dataset of 4,724 abstracts
#or 2,764 abstracts if we leave out country!

#pull out the countries each species is found in: 
#load the species/countries list: 
species_country <- read.csv("SpeciesAmericas_v2.csv",header=TRUE,encoding = "UTF-8")

#tokenize the dataset to include bigrams and single words for country and species:
#first with species:
species_country <- species_country %>%
  unnest_tokens(species, Species, token = "ngrams", n = 2)
species_country <- species_country %>%
  unnest_tokens(country, Country) 
#issue: some countries grouped into "south cone" "CeAm", etc.
#resolved: just converted countries to regions later in this code

#this was test code, I'm not using it:
#species_country %>% 
  
  
#  plyr::ldply(data.frame) %>% # Change the list to a dataframe (See https://stackoverflow.com/questions/4227223/r-list-to-data-frame)
#  separate(Word_DefNumber, c("Word", "DefNumber"), sep = "\\.") %>% # Find the word part of the word and definition number
#  group_by(Word) %>% # Group by words, so that when we select rows it is done for each word
#  slice(1:5) %>% # Keep the first 5 rows for each word
#  summarise(synonyms = paste(Syn, collapse = ", ")) %>% # Combine the synonyms together comma separated using paste 
#  ungroup() # So there are not unintended effects of having the data grouped when using the data later




#pull out the country associated with authors: 
library(countrycode)
all_country <- countrycode::countryname_dict %>% 
  filter(grepl('[A-Za-z]', country.name.alt)) %>%
  pull(country.name.alt) %>% 
  tolower()
pattern <- str_c(all_country, collapse = '|')

pub_bias_filtered_no_dupes$Addresses2 <- pub_bias_filtered_no_dupes$Addresses
pub_bias_filtered_no_dupes2$Addresses2 <- pub_bias_filtered_no_dupes2$Addresses

df <- pub_bias_filtered_no_dupes %>%
  mutate(auth_country = str_extract_all(tolower(Addresses2), pattern)) %>%
  select(-Addresses2) %>%
  unnest(auth_country, keep_empty = TRUE)
country_list_test <- data.frame(country=unique(df$auth_country))
write.csv(country_list_test, "author-countries.csv")
#now I go through this dataset I just created and manually erase all the countries that don't sound real
#read that back in:
auth_country_dict <- read.csv("author-countries-clean2.csv",header=FALSE)

#re-run the country search:
auth_dict <- c(t(auth_country_dict))
pattern <- str_c(auth_dict, collapse = '|')
df <- pub_bias_filtered_no_dupes %>%
  mutate(auth_country = str_extract_all(tolower(Addresses2), pattern)) %>%
  select(-Addresses2) %>%
  unnest(auth_country, keep_empty = TRUE)
dfv2 <- pub_bias_filtered_no_dupes2 %>%
  mutate(auth_country = str_extract_all(tolower(Addresses2), pattern)) %>%
  select(-Addresses2) %>%
  unnest(auth_country, keep_empty = TRUE)

#remove duplicates:
df_no_dupes <- df[!duplicated(df[c("index","country_species_match", "auth_country")]),]
write.csv(df_no_dupes, "dataset-with-author-countries.csv")
df_no_dupesv2 <- dfv2[!duplicated(dfv2[c("index","bigram", "auth_country")]),]
write.csv(df_no_dupesv2, "dataset-with-author-countries-2.csv")






#pull out the countries associated with first authors: 
df_no_dupes$Reprint.Addresses2 <- df_no_dupes$Reprint.Addresses
df_no_dupesv2$Reprint.Addresses2 <- df_no_dupesv2$Reprint.Addresses

df2 <- df_no_dupes %>%
  mutate(cor_auth_country = str_extract_all(tolower(Reprint.Addresses2), pattern)) %>%
  select(-Reprint.Addresses2) %>%
  unnest(cor_auth_country, keep_empty = TRUE)
unique(df2$cor_auth_country)
write.csv(df2, "dataset-species-authors.csv")

df2v2 <- df_no_dupesv2 %>%
  mutate(cor_auth_country = str_extract_all(tolower(Reprint.Addresses2), pattern)) %>%
  select(-Reprint.Addresses2) %>%
  unnest(cor_auth_country, keep_empty = TRUE)
unique(df2v2$cor_auth_country)





#pull out a list of species represented in the dataset: 
dataset_species_list <- data.frame(species = unique(df2$country_species_match))
write.csv(dataset_species_list, "dataset_species_list.csv")

dataset_species_list2 <- data.frame(species = unique(df2v2$bigram))
write.csv(dataset_species_list2, "dataset_species_list2.csv")



#add in native range for each species:
#NOTE that PR is coded carribean for species, but USA for author
#because PR categorizes as USA in WoS
#not sure what to do about it
#condense all the countries each species is found into one string:
species_list <- species_list %>%
  group_by(Species) %>%
  summarise(Country=toString(Country))
#I keep getting an error and I'm tired, so I'm just going to convert to lowercase in excel:
write.csv(species_list, "Species-by-country.csv")
#excel magic
species_list <- read.csv("Species-by-country.csv",header=TRUE)
species_list <- as_tibble(species_list)
df2$species_native_range <- species_list$native_range[match(df2$country_species_match, species_list$species)]
df2v2$species_native_range <- species_list$native_range[match(df2v2$bigram, species_list$species)]


#binary match between first author and species country:
#first convert author country to region:
region_dict <- read.csv("author-regions-clean.csv",header=TRUE)
df2$cor_auth_region <- region_dict$region[match(df2$cor_auth_country, region_dict$country)]
df2v2$cor_auth_region <- region_dict$region[match(df2v2$cor_auth_country, region_dict$country)]

#next, create a binary column to look for matches between author and species range:
df2$cor_auth_species_match <- lengths(Map(intersect, strsplit(df2$cor_auth_region, ", "), 
                        strsplit(df2$species_native_range, ", "))) > 0
df2v2$cor_auth_species_match <- lengths(Map(intersect, strsplit(df2v2$cor_auth_region, ", "), 
                                          strsplit(df2v2$species_native_range, ", "))) > 0



#binary match between any author and species country:
df2$auth_region <- region_dict$region[match(df2$auth_country, region_dict$country)]
df2$auth_species_match <- lengths(Map(intersect, strsplit(df2$auth_region, ", "), 
                                          strsplit(df2$species_native_range, ", "))) > 0


df2v2$auth_region <- region_dict$region[match(df2v2$auth_country, region_dict$country)]
df2v2$auth_species_match <- lengths(Map(intersect, strsplit(df2v2$auth_region, ", "), 
                                      strsplit(df2v2$species_native_range, ", "))) > 0

write.csv(df2v2, "dataset-species-filter-only.csv")
#now go through and correct the NAs by hand in excel
#read it back in and re-run the matches:
#Thought: what about species found in both north america and europe?
#like vaccinium vitis-idaea
df2v3<-read.csv("dataset-species-filter-only-corrected.csv",header=TRUE)
#re-run the matches:
df2v3$cor_auth_species_match <- lengths(Map(intersect, strsplit(df2v3$cor_auth_region, ", "), 
                                            strsplit(df2v3$species_native_range, ", "))) > 0
df2v3$auth_species_match <- lengths(Map(intersect, strsplit(df2v3$auth_region, ", "), 
                                        strsplit(df2v3$species_native_range, ", "))) > 0
write.csv(df2v3, "dataset-species-only-final.csv")


#species-first author match:
#test:
df2v3 %>% group_by(index) %>% 
  summarise(total_match = sum(cor_auth_species_match == "TRUE"),
            total_mimsmatch = sum(cor_auth_species_match == "FALSE"),
            .groups = 'drop')
#or:
summarydf <- df2v3 %>% 
  group_by(index) %>%
  mutate(value_sum = case_when(cor_auth_species_match == "TRUE" ~ 1,
                               cor_auth_species_match == "FALSE" ~ 0)) %>% 
  summarise(cor_match = sum(value_sum))
summarydf <- summarydf %>%
  group_by(index) %>%
  mutate(cor_match = case_when(cor_match >= 1 ~ "TRUE",
                               cor_match == 0 ~ "FALSE"))

  


#species-any author match:
#I'm testing this code first, ignore:
test1 <- data.frame(index = c(1, 2, 2, 3, 3, 4, 4, 5),
                    value1 = c("TRUE", "FALSE", "FALSE", "FALSE", "FALSE", 
                              "TRUE", "TRUE", "FALSE"), 
                    value2 = c("FALSE", "TRUE", "TRUE", "FALSE", "FALSE",
                               "TRUE", "TRUE", "TRUE")) 
summarydftest <- test1 %>% 
  group_by(index) %>%
  mutate(cor_sum = case_when(value1 == "TRUE" ~ 1,
                             value1 == "FALSE" ~ 0)) %>% 
  mutate(auth_sum = case_when(value2 == "TRUE" ~ 1,
                              value2 == "FALSE" ~ 0)) %>% 
  summarise(cor_match = sum(cor_sum), 
            auth_match = sum(auth_sum))
summarydftest <- summarydftest %>%
  group_by(index) %>%
  mutate(total_match = case_when(
    cor_match >= 1 & auth_match >= 1 ~ "both",
    cor_match == 0 & auth_match == 0 ~ "neither",
    cor_match >= 1 & auth_match == 0 ~ "first_only",
    cor_match == 0 & auth_match >= 1 ~ "sec_only"
  ))
#okay that worked, now for real:
summarydf2 <- df2v3 %>% 
  group_by(index) %>%
  mutate(cor_sum = case_when(cor_auth_species_match == "TRUE" ~ 1,
                             cor_auth_species_match == "FALSE" ~ 0)) %>% 
  mutate(auth_sum = case_when(auth_species_match == "TRUE" ~ 1,
                              auth_species_match == "FALSE" ~ 0)) %>% 
  summarise(cor_match = sum(cor_sum), 
            auth_match = sum(auth_sum))
summarydf2 <- summarydf2 %>%
  group_by(index) %>%
  mutate(total_match = case_when(
    cor_match >= 1 & auth_match >= 1 ~ "both",
    cor_match == 0 & auth_match == 0 ~ "neither",
    cor_match >= 1 & auth_match == 0 ~ "first_only",
    cor_match == 0 & auth_match >= 1 ~ "sec_only"
  ))
summarydf2 <- summarydf2 %>%
  group_by(index) %>%
  mutate(any_match = case_when(
    total_match == "both" ~ "TRUE",
    total_match == "first_only" ~ "TRUE",
    total_match == "sec_only" ~ "TRUE", 
    total_match == "neither" ~ "FALSE"
  ))


#add that info back to the main dataset: 
data_merged <- df2v3 %>%
  group_by(index) %>%
  summarise(bigram = paste(unique(bigram), collapse = ","), 
            auth_country = paste(unique(auth_country), collapse = ","), 
            cor_auth_country = paste(unique(cor_auth_country), collapse = ","), 
            species_native_range = paste(unique(species_native_range), collapse = ","), 
            cor_auth_region = paste(unique(cor_auth_region), collapse = ","), 
            auth_region = paste(unique(auth_region), collapse = ","))
df_all <- left_join(data_merged, summarydf2, by = "index")
df_all <- rename(df_all, species = bigram)
df_all <- left_join(df_all, pub2, by = "index")
write.csv(df_all, "final-dataset-pb.csv")  



#graphs
clean_df <- read.csv("final-dataset-pb-clean-2.csv",header=TRUE)
clean_df %>% 
  count(cor_auth_country) %>% 
  ggplot(aes(x = reorder(cor_auth_country,(-n)), y = n)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Country of first author", y="Number of abstracts")
ggsave("all-abstracts-count.png",dpi=300)

clean_df %>% 
  count(cor_auth_region) %>% 
  ggplot(aes(x = reorder(cor_auth_region,(-n)), y = n)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Country/Region of first author", y="Number of abstracts")
ggsave("all-abstracts-regions-count.png",dpi=300)

clean_df %>% 
  count(cor_auth_country) %>% 
  filter(n > 10) %>%
  ggplot(aes(x = reorder(cor_auth_country, (-n)), y=n)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Country of first author", y="Number of abstracts (>10)")
ggsave("all-abstracts-count-over-10.png",dpi=300)

clean_df %>% 
  count(cor_auth_region) %>% 
  filter(n > 10) %>%
  ggplot(aes(x = reorder(cor_auth_region, (-n)), y=n)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Country/Region of first author", y="Number of abstracts (>10)")
ggsave("all-abstracts-count-over-10-regions.png",dpi=300)

clean_df %>% 
  filter(any_match==TRUE) %>%
  count(cor_auth_country) %>% 
  filter(n>1) %>%
  ggplot(aes(x = reorder(cor_auth_country, (-n)), y=n)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Country of first author for any match", y="Number of abstracts")
ggsave("match-abstracts-count.png",dpi=300)

clean_df %>% 
  filter(any_match==TRUE) %>%
  count(cor_auth_region) %>% 
  filter(n>1) %>%
  ggplot(aes(x = reorder(cor_auth_region, (-n)), y=n)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Country/Region of first author for any match", y="Number of abstracts")
ggsave("match-abstracts-count-regions.png",dpi=300)

clean_df %>% 
  filter(any_match==FALSE) %>%
  count(cor_auth_country) %>% 
  filter(n>1) %>%
  ggplot(aes(x = reorder(cor_auth_country, (-n)), y=n)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Country of first author for complete mismatch", y="Number of abstracts")
ggsave("mismatch-abstracts-count.png",dpi=300)

clean_df %>% 
  filter(any_match==FALSE) %>%
  count(cor_auth_region) %>% 
  filter(n>1) %>%
  ggplot(aes(x = reorder(cor_auth_region, (-n)), y=n)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Country/Region of first author for complete mismatch", y="Number of abstracts")
ggsave("mismatch-abstracts-count-region.png",dpi=300)

clean_df %>% 
  filter(total_match=="sec_only") %>%
  count(cor_auth_country) %>% 
  filter(n>1) %>%
  ggplot(aes(x = reorder(cor_auth_country, (-n)), y=n)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Country of first author for secondary author matches only", y="Number of abstracts")
ggsave("secondary-match-abstracts-count.png",dpi=300)

clean_df %>% 
  filter(total_match=="sec_only") %>%
  count(cor_auth_region) %>% 
  filter(n>1) %>%
  ggplot(aes(x = reorder(cor_auth_region, (-n)), y=n)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Country/region of first author for secondary author matches only", y="Number of abstracts")
ggsave("secondary-match-abstracts-count-regions.png",dpi=300)



clean_df %>% 
  filter(any_match==TRUE) %>%
  count(cor_auth_country) %>% 
  filter(n>1) %>%
  ggplot(aes(x = reorder(cor_auth_country, (-n)), y=n)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Country of first author for any match", y="Number of abstracts")


#by year:
#raw numbers:
p1 <- clean_df %>% 
  filter(any_match==TRUE) %>%
  count(Publication.Year) %>% 
  filter(n>1) %>%
  ggplot(aes(x = Publication.Year, y=n)) + 
  geom_bar(stat = 'identity') + 
  ylim(0,150) + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y="Any match", title = "Number of abstracts by author relationship to species")
p2 <- clean_df %>% 
  filter(total_match=="sec_only") %>%
  count(Publication.Year) %>% 
  filter(n>1) %>%
  ggplot(aes(x = Publication.Year, y=n)) + 
  geom_bar(stat = 'identity') + 
  ylim(0,150) + 
  theme_minimal() + 
  labs(x="Publication year", y="Sec. only match")
p3 <- clean_df %>% 
  filter(any_match==FALSE) %>%
  count(Publication.Year) %>% 
  filter(n>1) %>%
  ggplot(aes(x = Publication.Year, y=n)) + 
  geom_bar(stat = 'identity') + 
  ylim(0,150) + 
  theme_minimal() + 
  labs(x="Publication year", y="No match")
library(grid)
p4 <- rbind(ggplotGrob(p1), ggplotGrob(p3), size = "last")
grid.newpage()
grid.draw(p4)
tiff("abstract-counts-by-year.tiff",width=5, height=3, units="in", res=300) 
grid.draw(p4) 
dev.off()
#normalized by total per year:
counts_by_year <- read.csv("counts_by_year.csv",header=TRUE)
p1 <- counts_by_year %>% 
  ggplot(aes(x = Year, y=Any_match_perc)) + 
  geom_bar(stat = 'identity') + 
  ylim(0,1) + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y="Any match", title = "Percent of abstracts by author relationship to species")
p2 <- counts_by_year %>% 
  ggplot(aes(x = Year, y=Sec_only_perc)) + 
  geom_bar(stat = 'identity') + 
  ylim(0,1) + 
  theme_minimal() + 
  labs(x="Publication year", y="Sec. only match")
p3 <- counts_by_year %>% 
  ggplot(aes(x = Year, y=No_match_perc)) + 
  geom_bar(stat = 'identity') + 
  ylim(0,1) + 
  theme_minimal() + 
  labs(x="Publication year", y="No match")
counts_by_year2 <- read.csv("counts_by_year2.csv", header=TRUE)
ggplot(counts_by_year2, aes(x = Year, y = Percent, fill = Match)) +
  geom_col() + 
  scale_x_continuous(breaks=seq(1991, 2023, 2)) +
  labs(x="Publication Year", y="Percent") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
ggsave("matches_by_year.png",dpi=300)



#analyze relationships between author countries
auth_unnested <- clean_df %>%
  unnest_tokens(country, auth_country, token = 'regex', pattern=",")
library(widyr)
country_pairs <-auth_unnested %>%
  pairwise_count(country, index, sort = TRUE, upper=FALSE)
country_pairs
country_cors <- auth_unnested %>%
  group_by(country) %>%
  filter(n()>=2) %>%
  pairwise_cor(country, index, sort = TRUE)
country_cors
country_cors %>%
  filter(item1 == "usa")
country_cors %>%
  filter(item1 %in% c("usa", "brazil", "mexico", "china")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

library(ggraph)
library(igraph)
set.seed(1000)
country_pairs %>%
  filter(n >= 5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "lgl") +
  geom_edge_fan(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines"))
ggsave("author-associations-network.png",dpi=300)

country_locs <- read.csv("country_locs.csv", header=TRUE)
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))
node_pos <- country_locs %>%
  select(long, lat) %>%
  rename(x = long, y = lat)   # node positions must be called x, y
g<-country_pairs %>%
  graph_from_data_frame()
edges_for_plot <- country_pairs %>%
  inner_join(country_locs %>% select(Country, long, lat), by = c('item1' = 'Country')) %>%
  rename(x = long, y = lat) %>%
  inner_join(country_locs %>% select(Country, long, lat), by = c('item2' = 'Country')) %>%
  rename(xend = long, yend = lat)

lay <- create_layout(g, 'manual',
                     node.positions = node_pos)
assert_that(nrow(lay) == nrow(nodes))
# add node degree for scaling the node sizes
lay$weight <- degree(g)



#counts of authors per species in region
clean_df$species_native_range <- gsub(" ", "", clean_df$species_native_range)
species_unnested <- clean_df %>%
  unnest_tokens(country, species_native_range, token = 'regex', pattern=",")
species_unnested <- species_unnested[!duplicated(species_unnested[c("index","country")]),]
df1 <- species_unnested %>% count(country)
df2 <- species_unnested %>% group_by(country) %>% count(any_match)
df3 <- species_unnested %>% group_by(country) %>% count(total_match == "sec_only")
counts_by_region <- read.csv("counts_by_species_region_percents.csv",header=TRUE)

ggplot(counts_by_region, aes(x = Region, y = Percent, fill = Match)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x="Species Native Region", y="Percent") 
ggsave("matches_by_species_region.png",dpi=300)

#authors in Mexico, Ecuador
mex_auth <- clean_df %>%
  filter(grepl("mexico",auth_country))
write.csv(mex_auth, "author-from-mexico.csv")
ec_auth <- clean_df %>%
  filter(grepl("ecuador",auth_country))
write.csv(ec_auth, "author-from-ecuador.csv")
#species in Mexico, Ecuador
mex_species <- clean_df %>%
  filter(grepl("mexico",species_native_range))
write.csv(mex_species, "species-from-mexico.csv")
ec_species <- clean_df %>%
  filter(grepl("ecuador",species_native_range))
write.csv(ec_species, "species-from-ecuador.csv")

mex_auth_species <- with(clean_df, clean_df[grepl("mexico",auth_country) | grepl("mexico",species_native_range),])
mex_auth_species$mex_match <- 
  with(mex_auth_species, 
       str_detect(auth_country, "mexico") & 
         str_detect(species_native_range, "mexico")
  )
write.csv(mex_auth_species, "mexican-authors-or-species.csv")
mex_auth_species$mex_species <- 
  with(mex_auth_species, 
         str_detect(species_native_range, "mexico")
  )

ec_auth_species <- with(clean_df, clean_df[grepl("ecuador",auth_country) | grepl("ecuador",species_native_range),])
ec_auth_species$ec_match <- 
  with(ec_auth_species, 
       str_detect(auth_country, "ecuador") & 
         str_detect(species_native_range, "ecuador")
  )
write.csv(ec_auth_species, "ecuadorian-authors-or-species.csv")

mex_ec_auth_species <- with(clean_df, clean_df[grepl("ecuador",auth_country) | grepl("ecuador",species_native_range) | grepl("mexico",auth_country) | grepl("mexico",species_native_range),])
mex_ec_auth_species$mex_match <-
  with(mex_ec_auth_species, 
       str_detect(auth_country, "mexico") & 
         str_detect(species_native_range, "mexico")
  )
mex_ec_auth_species$ec_match <-
  with(mex_ec_auth_species, 
       str_detect(auth_country, "ecuador") & 
         str_detect(species_native_range, "ecuador")
  )
mex_ec_auth_species$mex_species <- with(mex_ec_auth_species, str_detect(species_native_range, "mexico"))
mex_ec_auth_species$ec_species <- with(mex_ec_auth_species, str_detect(species_native_range, "ecuador"))
write.csv(mex_ec_auth_species, "species-or-author-from-mex-or-ec.csv")

p1<-mex_ec_auth_species %>%
  filter(ec_species == "TRUE") %>%
  ggplot(., mapping = aes(x = ec_match)) +
  geom_bar() + 
  labs(x = "Author and Species both from Ecuador", title = "Papers on Ecuadorian species")
ggsave("ecuadorian-species.jpg",dpi=300)

p2<-mex_ec_auth_species %>%
  filter(mex_species == "TRUE") %>%
  ggplot(., mapping = aes(x = mex_match)) +
  geom_bar() + 
  labs(x = "Author and Species both from Mexico", title = "Papers on Mexican species")
ggsave("mexican-species.jpg",dpi=300)

library(gridExtra)
p1<-mex_ec_auth_species %>%
  filter(ec_species == "TRUE") %>%
  ggplot(., mapping = aes(x = ec_match)) +
  geom_bar() + 
  ylim(0,1000) + 
  labs(x = "Ecuadorian Author")
p2<-mex_ec_auth_species %>%
  filter(mex_species == "TRUE") %>%
  ggplot(., mapping = aes(x = mex_match)) +
  geom_bar() + 
  ylim(0,1000) + 
  labs(x = "Mexican Author") + 
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())
grid.arrange(p1, p2, top = "Papers on Species from Target Country", ncol=2)
ggsave("mexico-ecuador.jpg", dpi=300, arrangeGrob(p1, p2, ncol=2, top="Papers on species from target country"))
