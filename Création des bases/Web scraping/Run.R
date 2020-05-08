pacman::p_load(rvest,
               rio,
               foreach,
               httr,
               tidyverse)

DATA <- function(liste,
                 ...){
  
  data = liste %>%
    GET(.,
        timeout(1000)) %>%
    read_html
  
  return(data)
}


liste = "C:/Users/caren/Desktop/Extraction/caren_R/Liste_Acteurs.rds" %>% import

liste = liste %>%
  mutate(page_perso = str_replace_all(string = page_perso,
                                      pattern = "&apos;",
                                      replacement = "'"),
         page_perso = str_replace_all(string = page_perso,
                                      pattern = "\\%2d",
                                      replacement = "-"),
         page_perso = str_replace_all(string = page_perso,
                                      pattern = "\\%2e",
                                      replacement = "."),
         page_perso = str_replace_all(string = page_perso,
                                      pattern = "\\%1a(?=nna-camay.htm)",
                                      replacement = "A")) %>%
  filter(!page_perso == "http://www.iafd.com//person.rme/perfid=freakydeek3/gender=m/%24cam-likely.htm")

#liste = header(liste, n = 1000) %>% 
#  filter(!page_perso %in% c("http://www.iafd.com//person.rme/perfid=um536379a/gender=m/unknown-male-536379%2da.htm",
#                            "http://www.iafd.com//person.rme/perfid=um536379b/gender=m/unknown-male-536379%2db.htm",
#                            "http://www.iafd.com//person.rme/perfid=crisdhulg/gender=f/cris-d&apos;hulg.htm",
#                            "http://www.iafd.com//person.rme/perfid=darclyte/gender=m/d%2e-arclyte.htm"))

data = liste$page_perso %>% 
  map(.x = .,
      .f = ~ DATA(liste = .))

test = data %>% 
  set_names(liste$page_perso) %>% 
  map(~html_text(html_nodes(x = .,
                            css = "#top"))) %>%
  unlist %>% 
  enframe(value = "Name",
          name = "Link_perso")

informations = c("Alias", "Birthday",
                 "Astrology", "Birthplace",
                 "YearsActive", "Ethnicity",
                 "Nationality", "HairColor",
                 "Height", "Weight",
                 "Measurements", "Tattoos",
                 "Piercings", "Alias",
                 "Birthday", "Website",
                 "HairColors", "YearActive",
                 "DateofDeath","DirectorAKA",
                 "YearsActiveasDirector",
                 "YearActiveasDirector",
                 "YearActiveasPerformer",
                 "YearsActiveasPerformer",
                 "PhotoGalleries","VideoGalleries")

test0 = data %>%
  set_names(liste$page_perso) %>% 
  map(~html_text(html_nodes(x = .,
                            css = ".bioheading"))) %>% 
  unlist() %>% 
  enframe(value = "Information") %>% 
  mutate(Information = str_replace_all(string = Information,
                                       pattern = '\\s*',
                                       replacement = ""),
         Information1 = if_else(Information %in% c("PerformerAKA","AKA"),
                                 "Alias", 
                                 Information),
         Information2 = if_else(Information1 %in% informations,
                                Information1,
                                paste("Award:",
                                      Information1,
                                      sep = " ")),
         Link_perso = if_else(str_detect(string = name,
                                         pattern = '\\d*$') == TRUE,
                              str_replace(string = name,
                                          pattern = '\\d*$',
                                          replacement = ""),
                              name)) %>% 
  select(-name,
         -Information,
         -Information1) %>%
  filter(!Information2 %in% c("PhotoGalleries",
                             "VideoGalleries"))

test1 = test0 %>% 
  filter(!str_detect(string = Information2,
                     pattern = "^(Award|Website)"))

test11 = test1 %>%
  mutate(pseudo = str_extract(string = Link_perso,
                             pattern = "(?<=perfid=)\\w+")) %>%
  group_by(Link_perso) %>% 
  mutate(number = 1, 
         number2 = cumsum(number),
         pseudo = paste0(pseudo, 
                         number2)) %>% 
  select(-number, 
         -number2)

## Récupérer les values

test00 = data %>%
  set_names(liste$page_perso) %>% 
  map(~html_text(html_nodes(x = .,
                            css = ".biodata"))) %>%
  unlist() %>% 
  enframe() %>% 
  mutate(Link_perso = if_else(str_detect(string = name,
                                         pattern = '\\d*$') == TRUE,
                              str_replace(string = name,
                                          pattern = '\\d*$',
                                          replacement = ""),
                              name)) %>% 
  select(-name)

test2 = test00 %>% 
  filter(!str_detect(string = value,
                     pattern = "^(http|Nominee|Winner)"))

test22 = test2 %>%
  mutate(pseudo = str_extract(string = Link_perso,
                              pattern = "(?<=perfid=)\\w+")) %>%
  group_by(Link_perso) %>% 
  mutate(number = 1, 
         number2 = cumsum(number),
         pseudo = paste0(pseudo, 
                         number2)) %>% 
  select(-number, 
         -number2)

test3 = test0 %>% 
  anti_join(test1) %>% 
  mutate(jointure = if_else(Information2 == "Website",
                            "Site",
                            "Award"))

test4 = test00 %>% 
  anti_join(test2) %>% 
  mutate(jointure = if_else(str_detect(string = value,
                                       pattern = "^http"),
                            "Site",
                            "Award"))
#rm(test0,test00,test1,test2)

data5 = test3 %>% 
  left_join(test4,
            by = c("jointure",
                   "Link_perso")) %>% 
  group_by(Information2,
           Link_perso) %>% 
  mutate(info_group = paste0(value, 
                             collapse = ", ")) %>%
  select(-jointure,
         -value) %>% 
  distinct() %>% 
  pivot_wider(names_from = Information2,
              values_from = info_group)

data3 = left_join(test11,
                  test22,
                  by = c("pseudo",
                         "Link_perso")) %>% 
  select( -pseudo) %>%
  pivot_wider(names_from = Information2,
              values_from = value)

#dat = head(data3, n = 100)

data4 = data3 %>% 
  mutate(YearsActive = if_else(is.na(YearsActive) == TRUE &
                                 is.na(YearActive) == FALSE,
                               YearActive,
                               YearsActive),
         HairColors = if_else(is.na(HairColors) == TRUE &
                                is.na(HairColor) == FALSE,
                              HairColor,
                              HairColors),
         Age = str_replace_all(string = str_extract_all(string = Birthday,
                                                        pattern = "\\(([^)]+)\\)"),
                               pattern = "\\D",
                               replacement = ""),
         Age = if_else(Age == "0",
                       NA_character_,
                       Age),
         StartAge = str_replace_all(string = str_extract_all(string = YearsActive,
                                                             pattern = "\\(([^)]+)\\)"),
                                    pattern = "\\D",
                                    replacement = ""),
         StartAge = if_else(StartAge == "0",
                            NA_character_,
                            StartAge),
         Taille_cm = str_replace_all(string = str_extract_all(string = Height,
                                                              pattern = "\\(([^)]+)\\)"),
                                     pattern = "\\D",
                                     replacement = ""),
         Taille_cm = if_else(Taille_cm == "0",
                             NA_character_,
                             Taille_cm),
         Poids_kg = str_replace_all(string = str_extract_all(string = Weight,
                                                             pattern = "\\(([^)]+)\\)"),
                                    pattern = "\\D",
                                    replacement = ""),
         Poids_kg = if_else(Poids_kg == "0",
                            NA_character_,
                            Poids_kg),
         Gender = str_extract(string = Link_perso,
                              pattern = "(?<=gender=)\\w")) %>% 
  mutate_at(c("Height",
              "Weight",
              "Birthday",
              "YearsActive"), 
            ~ str_replace_all(string = .,
                              pattern = "\\(([^)]+)\\)",
                              replacement = "")) %>% 
  mutate_at(c("Nationality",
              "Ethnicity",
              "HairColors"),
            ~ str_replace_all(string = .,
                              pattern = '\\/',
                              replacement = ", ")) %>%
  mutate_at(c("Nationality",
              "Ethnicity",
              "HairColors"),
            ~ str_to_title(.)) %>% 
  select(-YearActive,
         -HairColor)

data5 = test %>% 
  left_join(data4,
            by = "Link_perso") %>% 
  left_join(data5,
            by = "Link_perso")

data5 %>%
  export("C:/Users/caren/Desktop/Extraction/caren_R/Porn_Acteurs3.rds")
