library(ggplot2)
library(data.table)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(xlsx)
library(plotly)
library(kableExtra)
library(DT)
library(RMariaDB)
library(DBI)
library(stringr)
library(stringi)


## DATA IMPORT 
data_map = readRDS("fp_map (1).rds")

data_receipts = readRDS("fp_receipts.rds")

n <- nrow(data_receipts)

colnames(data_receipts)
part1 <- data_receipts[1:(n/2), ]
part2 <- data_receipts[(n/2 + 1):n, ]

View(part1)

sum(is.na(part1))

sum(is.na(part2))


#5000454
dim(part1)

#5000454
dim(part2)

dim(data_receipts)

####Data Cleaning#######

part1$pos = toupper(part1$pos)

part2$pos = toupper(part2$pos)

part1$product_name = toupper(part1$product_name)

part2$product_name = toupper(part2$product_name)

part1$pos = str_squish(part1$pos)

part2$pos = str_squish(part2$pos)


part1$product_name = str_squish(part1$product_name)

part2$product_name = str_squish(part2$product_name)


part1$pos = trimws(part1$pos)

part2$pos = trimws(part2$pos)


View(part1)


part1$pos <- gsub("\\\\", "",part1$pos)

part2$pos <- gsub("\\\\", "",part2$pos)


part1$pos <- gsub('""', "",part1$pos)

part2$pos <- gsub('""', "",part2$pos)


part1$pos = gsub('"\"',"",part1$pos)

part2$pos = gsub('"\"',"",part2$pos)



part1 <- part1 %>%
  mutate(
    pos = pos %>%
      str_replace_all(c(
        "İ" = "I",
        "Ə" = "E",
        "\\?" = "E",
        "Ğ" = "G",
        "Ü" = "U",
        "Ş" = "S",
        "Ç" = "C",
        "Ö" = "O"
      ))
  )


part2 <- part2 %>%
  mutate(
    pos = pos %>%
      str_replace_all(c(
        "İ" = "I",
        "Ə" = "E",
        "\\?" = "E",
        "Ğ" = "G",
        "Ü" = "U",
        "Ş" = "S",
        "Ç" = "C",
        "Ö" = "O"
      ))
  )



part1 <- part1 %>%
  mutate(
    product_name = product_name %>%
      str_replace_all(c(
        "İ" = "I",
        "Ə" = "E",
        "\\?" = "E",
        "Ğ" = "G",
        "Ü" = "U",
        "Ş" = "S",
        "Ç" = "C",
        "Ö" = "O"
      ))
  )


part2 <- part2 %>%
  mutate(
    product_name = product_name %>%
      str_replace_all(c(
        "İ" = "I",
        "Ə" = "E",
        "\\?" = "E",
        "Ğ" = "G",
        "Ü" = "U",
        "Ş" = "S",
        "Ç" = "C",
        "Ö" = "O"
      ))
  )

sum(is.na(part1))

sum(is.na(part2))


View(part1)


part1 = part1 %>%
  mutate(
    Total = part1$unit_price * part1$qnt
  )

part1 <- part1 %>%
  mutate(hefte_gunu = wday(rec_date, label = TRUE, abbr = FALSE))


part2 = part2 %>%
  mutate(
    Total = part2$unit_price * part2$qnt
  )

part2 <- part2 %>%
  mutate(hefte_gunu = wday(rec_date, label = TRUE, abbr = FALSE))


part1$product_name <- str_replace_all(
  part1$product_name,
  "[_,.\"()/]", " "
)

part2$product_name <- str_replace_all(
  part2$product_name,
  "[_,.\"()/]", " "
)


View(part1)

part1$product_name <- str_squish(part1$product_name)

part2$product_name <- str_squish(part2$product_name)

part1$product_name <- part1$product_name %>%
  str_replace_all("\\bQR\\b|\\bGR\\b|\\bG\\b", "GR") %>%
  str_replace_all("\\bML\\b", " ML") %>%
  str_replace_all("\\bLT\\b|\\bL\\b", "L") %>%
  str_replace_all("\\bKQ\\b|\\bKG\\b", "KG")

part2$product_name <- part2$product_name %>%
  str_replace_all("\\bQR\\b|\\bGR\\b|\\bG\\b", "GR") %>%
  str_replace_all("\\bML\\b", " ML") %>%
  str_replace_all("\\bLT\\b|\\bL\\b", "L") %>%
  str_replace_all("\\bKQ\\b|\\bKG\\b", "KG")




unique(part1$product_name)


View(part1)

View(part2)

#5000454       
dim(part1)

#5000454       
dim(part2)

part1_problem = part1 %>%
  filter(product_name == '')

View(part1_problem)


part2_problem = part1 %>%
  filter(product_name == '')

View(part2_problem)

part1 = part1 %>%
  filter(
    !(product_name == '')
  )

part2 = part2 %>%
  filter(
    !(product_name == '')
  )

##4993749       
dim(part1)

##4992653       
dim(part2)

sum(is.na(part1))

sum(is.na(part2))


data_test = part1 %>%
  filter(product_name == '†')

View(data_test)

unique(part1$product_name)


###############################
# Sebeke sutunu

part1 <- part1 %>%
  mutate(
    group = case_when(
      str_detect(pos, "ARAZ") ~ "ARAZ",
      str_detect(pos, "BRAVO") ~ "BRAVO",
      str_detect(pos, "OBA") ~ "OBA",
      str_detect(pos, "NEPTUN") ~ "NEPTUN",
      str_detect(pos,"5+") ~ "5+",
      str_detect(pos,"BAZARSTORE|BAZAR STORE") ~ "BAZARSTORE",
      str_detect(pos,"ANBAR|AMBAR|DEPO") ~ "ANBAR",
      str_detect(pos,"AL MARKET|AL MERKET") ~ "AL",
      str_detect(pos,"GRAND|GRANMART") ~ "GRANDMART",
      str_detect(pos,"BOLMART") ~ "BOLMART",
      str_detect(pos,"AVROMART") ~ "AVROMART",
      str_detect(pos,"RAHAT") ~ "RAHAT",
      str_detect(pos,"KING SMART|KINGSMART") ~ "KİNG SMART",
      str_detect(pos,"SPAR") ~ "SPAR",
      str_detect(pos,"MEGASTORE") ~ "MEGASTORE",
      str_detect(pos,"FAVORIT") ~ "FAVORIT",
      str_detect(pos,"BIZIM") ~ "BIZIM",
      ################ GEYİM & BREND MAĞAZALARI
      str_detect(pos,"ADIDAS") ~ "ADIDAS",
      str_detect(pos,"ZARA") ~ "ZARA",
      str_detect(pos,"WAIKIKI GEYIM MAGAZASI|LCW") ~ "WAIKIKI",
      str_detect(pos,"DE FACTO") ~ "DE FACTO",
      str_detect(pos,"GO SPORT") ~ "GO SPORT",
      str_detect(pos,'NYU YORKER|NEW YORKER') ~ "NYU YORKER",
      str_detect(pos,'COURIR') ~ "COURIR",
      str_detect(pos,"SUVARI") ~ "SÜVARİ",
      str_detect(pos,"AVVA") ~ "AVVA",
      str_detect(pos,"ARMANI") ~ "ARMANI",
      str_detect(pos,"FLO") ~ "FLO",
      str_detect(pos,"GUCCI") ~ "GUCCI",
      str_detect(pos,"PANDA") ~ "PANDA",
      str_detect(pos,'TERRANOVA') ~ "TERRANOVA",
      str_detect(pos,'BERSHKA') ~ "BERSHKA",
      str_detect(pos,'RALPH LAUREN') ~ "RALPH LAUREN",
      str_detect(pos,'US POLO') ~ "US POLO",
      str_detect(pos,'CINICI') ~ "ÇINICI",
      str_detect(pos,'PULL AND BEAR') ~ 'PULL AND BEAR',
      str_detect(pos,'RODRIGO') ~ 'RODRIGO',
      str_detect(pos,'VILLEROY BOCH') ~ 'VILLEROY BOCH',
      str_detect(pos,"SEIKO|TISSOT|G-SHOCK|VMF") ~ "SAAT",
      str_detect(pos,'CHARLES AND KEITH') ~ 'CHARLES AND KEİTH',
      ###############
      str_detect(pos,"KFC") ~ "KFC",
      str_detect(pos,"PAPA") ~ "PAPA CONS",
      str_detect(pos,"BURGER KING") ~ "BURGER KING",
      str_detect(pos,"PIZZA HUT") ~ "PIZZA HUT",
      str_detect(pos,"PANDORA") ~ "PANDORA",
      str_detect(pos,"MCDONALDS|DONALD") ~ "MCDONALDS",
      str_detect(pos,"PIDEM") ~ "PIDEM",
      str_detect(pos,"RESTORAN|MADO|RESORAN|RESTAURANT|MOVIDA|BEER ART|NAMLI KABAB|ICTIMAI IASE|PIVBAR|BAKE AND ROLL|SUSHI ROOM|SAGI BAR") ~ "RESTORAN",
      str_detect(pos,"CAFE|KAFE|LOUNGE|DONER") ~ "KAFE",
      #################
      str_detect(pos,'OPTIMAL') ~ "OPTIMAL",
      str_detect(pos,'KONTAKT') ~ 'KONTAKT',
      str_detect(pos,'IRSAD') ~ 'İRŞAD',
      str_detect(pos,'BAKU ELEKTRONIKS|BAKU ELECTRONICS') ~ "BAKU ELECTRONICS",
      ###################
      str_detect(pos,"KARACA") ~ "KARACA",
      str_detect(pos,"LIBRAFF") ~ "LİBRAFF",
      str_detect(pos,"APTEK|SKY-100") ~ "APTEK",
      str_detect(pos,"MADAM COCO|COCO") ~ "MADAM COCO",
      str_detect(pos,'STARBUCKS|COFE|COFFEE|KOFE|COFE STORE') ~ 'COFFESHOPS',
      str_detect(pos,"MARKET|PREMIUM|MAGZA|MIKROMART|BAQQAL|DUKAN|DOSTMART|EMIL|ERZAQ|ERZAG|PARKET|SHOP|LOVE LOKUM|INSAAT|GALLERY|STORE|KONTINENTAL|KENAN|NERMIN|PRIBALTIKA|
                 MAQAZIN|QAYALI|MARTKET|EVI|MAGAZA|QABLAR EVI|DEFTERXANA LEVAZIMATLARI|1001|REBUS|MAQAZ") ~ "MAĞAZA/MARKET",
      str_detect(pos,"SEHIYYE MERKEZ|TALASSEMIYA|MEDICLUB|SAGLAMLIQ|HEALTH|MUALICƏ MERKEZI|MEDLAB|TIBB|MEDICAL|MEDICAL|HOSPITAL|XESTEXANA|KLINIK|CLINIC") ~ "XƏSTƏXANA",
      str_detect(pos,'KOSK|KIOSK') ~ "KÖŞK",
      str_detect(pos,"ZAVOD|SEX|FABRIK|ISTEHSAL") ~ "ZAVOD",
      str_detect(pos,"AVTOMOBIL|SERVIS|LEXUS|EHTIYYAT HISSELERI|SERVICE|AVTO|AUTO|KIA/HUNDAI EHT|VOLKSWAGEN") ~ "Avtomobil",
      str_detect(pos,"OBYEKT|SAUNA|ICARE|SUBICARE|IDMAN SAGLAMLIQ KOMPLEKSI|EYLENCE MERKEZI|SPA & FITNESS|AY ISIG|
                 SADLIQ EVI|AG SARAY|OFIS|OFFICE|FOR FIT|HAMAM|NARGILE|ISTIXANA") ~ "OBYEKT",
      str_detect(pos,"MMC|MEHDUD MESULIYYETLI CEMIYYET|MAYOMED|LTD") ~ "COMPANY",
      str_detect(pos,"OTEL|MEHMANXANA|ISTIRAHET|FLY INN BAKU") ~ "OTEL",
      str_detect(pos,"EMBAWOOD|MEBEL|ISTIKBAL|SELHOME|ASLANOGLU|SALOGLU|GOKTAS|BAMBI") ~ "Mebel Magazalari",
      str_detect(pos,"SATISI|SATIS|SATI") ~ "SATIŞ OBYEKTI",
      str_detect(pos,"TICARET|MALL|PLAZA|PORT BAKU|CRESENT|BAZAR|UNIVERMAQ|UNVERMAQ|UNIVERMAGI") ~ "TİCARƏT OBYEKTI",
      str_detect(pos,"BAKCELL|AZERCEL|MUSTERI XIDMETLERI") ~ "Musteri Xidmetleri",
      str_detect(pos,"STOMATOLOJI|STOMOTOLOYI|DENTBERG|STOMOTOLOGIYA|STOMOTOLOQ|DENTAL|STOMOTOLOYİ|STOMATOLOGIYA") ~ "STOMOTOLOGIYA",
      str_detect(pos,"OPTIK") ~ "OPTIKA",
      str_detect(pos,"SIRNIYYAT|PAXLAVA") ~ "ŞİRNİYYAT Magazalari",
      str_detect(pos,"ENTERTAINER|JOY TOYS|USAQ ALEMI") ~ "ENTERTAINER",
      str_detect(pos,"ALOE|COSMETIK|ADORE|SABINA|LASER|KOSMETOLOJI|ORGANIC|BELISSIMO|SABINA|COSMETICS") ~ "GOzellik Salonlari ve Kosmetika",
      str_detect(pos,"JYSK") ~ "JYSK",
      str_detect(pos,"LABARATORIYA|LABORATORIYA|DIAQNOSTIKA MERKEZI") ~ "LABORATORİYA",
      TRUE ~ "DIGER"
    )
  )

View(part1)

part2 <- part2 %>%
  mutate(
    group = case_when(
      
      str_detect(pos, "ARAZ") ~ "ARAZ",
      str_detect(pos, "BRAVO") ~ "BRAVO",
      str_detect(pos, "OBA") ~ "OBA",
      str_detect(pos, "NEPTUN") ~ "NEPTUN",
      str_detect(pos,"5+") ~ "5+",
      str_detect(pos,"BAZARSTORE|BAZAR STORE") ~ "BAZARSTORE",
      str_detect(pos,"ANBAR|AMBAR|DEPO") ~ "ANBAR",
      str_detect(pos,"AL MARKET|AL MERKET") ~ "AL",
      str_detect(pos,"GRAND|GRANMART") ~ "GRANDMART",
      str_detect(pos,"BOLMART") ~ "BOLMART",
      str_detect(pos,"AVROMART") ~ "AVROMART",
      str_detect(pos,"RAHAT") ~ "RAHAT",
      str_detect(pos,"KING SMART|KINGSMART") ~ "KİNG SMART",
      str_detect(pos,"SPAR") ~ "SPAR",
      str_detect(pos,"MEGASTORE") ~ "MEGASTORE",
      str_detect(pos,"FAVORIT") ~ "FAVORIT",
      str_detect(pos,"BIZIM") ~ "BIZIM",
      ################ GEYİM & BREND MAĞAZALARI
      str_detect(pos,"ADIDAS") ~ "ADIDAS",
      str_detect(pos,"ZARA") ~ "ZARA",
      str_detect(pos,"WAIKIKI GEYIM MAGAZASI|LCW") ~ "WAIKIKI",
      str_detect(pos,"DE FACTO") ~ "DE FACTO",
      str_detect(pos,"GO SPORT") ~ "GO SPORT",
      str_detect(pos,'NYU YORKER|NEW YORKER') ~ "NYU YORKER",
      str_detect(pos,'COURIR') ~ "COURIR",
      str_detect(pos,"SUVARI") ~ "SÜVARİ",
      str_detect(pos,"AVVA") ~ "AVVA",
      str_detect(pos,"ARMANI") ~ "ARMANI",
      str_detect(pos,"FLO") ~ "FLO",
      str_detect(pos,"GUCCI") ~ "GUCCI",
      str_detect(pos,"PANDA") ~ "PANDA",
      str_detect(pos,'TERRANOVA') ~ "TERRANOVA",
      str_detect(pos,'BERSHKA') ~ "BERSHKA",
      str_detect(pos,'RALPH LAUREN') ~ "RALPH LAUREN",
      str_detect(pos,'US POLO') ~ "US POLO",
      str_detect(pos,'CINICI') ~ "ÇINICI",
      str_detect(pos,'PULL AND BEAR') ~ 'PULL AND BEAR',
      str_detect(pos,'RODRIGO') ~ 'RODRIGO',
      str_detect(pos,'VILLEROY BOCH') ~ 'VILLEROY BOCH',
      str_detect(pos,"SEIKO|TISSOT|G-SHOCK|VMF") ~ "SAAT",
      str_detect(pos,'CHARLES AND KEITH') ~ 'CHARLES AND KEİTH',
      ###############
      str_detect(pos,"KFC") ~ "KFC",
      str_detect(pos,"PAPA") ~ "PAPA CONS",
      str_detect(pos,"BURGER KING") ~ "BURGER KING",
      str_detect(pos,"PIZZA HUT") ~ "PIZZA HUT",
      str_detect(pos,"PANDORA") ~ "PANDORA",
      str_detect(pos,"MCDONALDS|DONALD") ~ "MCDONALDS",
      str_detect(pos,"PIDEM") ~ "PIDEM",
      str_detect(pos,"RESTORAN|MADO|RESORAN|RESTAURANT|MOVIDA|BEER ART|NAMLI KABAB|ICTIMAI IASE|PIVBAR|BAKE AND ROLL|SUSHI ROOM|SAGI BAR") ~ "RESTORAN",
      str_detect(pos,"CAFE|KAFE|LOUNGE|DONER") ~ "KAFE",
      ################# Done
      str_detect(pos,'OPTIMAL') ~ "OPTIMAL",
      str_detect(pos,'KONTAKT') ~ 'KONTAKT',
      str_detect(pos,'IRSAD') ~ 'İRŞAD',
      str_detect(pos,'BAKU ELEKTRONIKS|BAKU ELECTRONICS') ~ "BAKU ELECTRONICS",
      ###################
      str_detect(pos,"KARACA") ~ "KARACA",
      str_detect(pos,"LIBRAFF") ~ "LİBRAFF",
      str_detect(pos,"APTEK|SKY-100") ~ "APTEK",
      str_detect(pos,"MADAM COCO|COCO") ~ "MADAM COCO",
      str_detect(pos,'STARBUCKS|COFE|COFFEE|KOFE|COFE STORE') ~ 'COFFESHOPS',
      str_detect(pos,"MARKET|PREMIUM|MAGZA|MIKROMART|BAQQAL|DUKAN|DOSTMART|EMIL|ERZAQ|ERZAG|SHOP|PARKET|LOVE LOKUM|INSAAT|GALLERY|STORE|KONTINENTAL|KENAN|NERMIN|PRIBALTIKA|
                 MAQAZIN|QAYALI|MARTKET|EVI|MAGAZA|QABLAR EVI|DEFTERXANA LEVAZIMATLARI|1001|REBUS|MAQAZ") ~ "MAĞAZA/MARKET",
      str_detect(pos,"SEHIYYE MERKEZ|TALASSEMIYA|MEDICLUB|SAGLAMLIQ|HEALTH|MUALICƏ MERKEZI|MEDLAB|TIBB|MEDICAL|MEDICAL|HOSPITAL|XESTEXANA|KLINIK|CLINIC") ~ "XƏSTƏXANA",
      str_detect(pos,'KOSK|KIOSK') ~ "KÖŞK",
      str_detect(pos,"ZAVOD|SEX|FABRIK|ISTEHSAL") ~ "ZAVOD",
      str_detect(pos,"AVTOMOBIL|SERVIS|LEXUS|EHTIYYAT HISSELERI|SERVICE|AVTO|AUTO|KIA/HUNDAI EHT|VOLKSWAGEN") ~ "Avtomobil",
      str_detect(pos,"OBYEKT|SAUNA|ICARE|SUBICARE|IDMAN SAGLAMLIQ KOMPLEKSI|EYLENCE MERKEZI|SPA & FITNESS|AY ISIG|
                 SADLIQ EVI|AG SARAY|OFIS|OFFICE|FOR FIT|HAMAM|NARGILE|ISTIXANA") ~ "OBYEKT",
      str_detect(pos,"MMC|MEHDUD MESULIYYETLI CEMIYYET|MAYOMED|LTD") ~ "COMPANY",
      str_detect(pos,"OTEL|MEHMANXANA|ISTIRAHET|FLY INN BAKU") ~ "OTEL",
      str_detect(pos,"EMBAWOOD|MEBEL|ISTIKBAL|SELHOME|ASLANOGLU|SALOGLU|GOKTAS|BAMBI") ~ "Mebel Magazalari",
      str_detect(pos,"SATISI|SATIS|SATI") ~ "SATIŞ OBYEKTI",
      str_detect(pos,"TICARET|MALL|PLAZA|PORT BAKU|CRESENT|BAZAR|UNIVERMAQ|UNVERMAQ|UNIVERMAGI") ~ "TİCARƏT OBYEKTI",
      str_detect(pos,"BAKCELL|AZERCEL|MUSTERI XIDMETLERI") ~ "Musteri Xidmetleri",
      str_detect(pos,"STOMATOLOJI|STOMOTOLOYI|DENTBERG|STOMOTOLOGIYA|STOMOTOLOQ|DENTAL|STOMOTOLOYİ|STOMATOLOGIYA") ~ "STOMOTOLOGIYA",
      str_detect(pos,"OPTIK") ~ "OPTIKA",
      str_detect(pos,"SIRNIYYAT|PAXLAVA") ~ "ŞİRNİYYAT Magazalari",
      str_detect(pos,"ENTERTAINER|JOY TOYS|USAQ ALEMI") ~ "ENTERTAINER",
      str_detect(pos,"ALOE|COSMETIK|ADORE|SABINA|LASER|KOSMETOLOJI|ORGANIC|BELISSIMO|SABINA|COSMETICS") ~ "GOzellik Salonlari ve Kosmetika",
      str_detect(pos,"JYSK") ~ "JYSK",
      str_detect(pos,"LABARATORIYA|LABORATORIYA|DIAQNOSTIKA MERKEZI") ~ "LABORATORİYA",
      TRUE ~ "DIGER"
    )
  )
View(part2)



data_optimal1 = part1 %>% 
  filter(str_detect(pos,"OPTIMAL"))
View(data_optimal1)




data_optimal2 = part2 %>% 
  filter(str_detect(pos,"OPTIMAL"))
View(data_optimal2)

# ALTARYUYAN VESTEL EDED 60000
# OPTIMAL ED   2 37.00
# SATIS EDED 2 90.99
# ZELMER EDED 2 210.99


data_kontakt2 = part2 %>% 
  filter(str_detect(pos,"KONTAKT"))
View(data_kontakt1)

# regemsal
# kredit
# ktedit
# xidmet
# MEBEL

data_kontakt1 = part1 %>% 
  filter(str_detect(pos,"KONTAKT"))
View(data_kontakt1)



data_irsad1 = part1 %>% 
  filter(str_detect(pos,"IRSAD"))
View(data_irsad1)



data_baku1 = part1 %>% 
  filter(str_detect(pos,"BAKU ELECTRONICS"))
View(data_baku1)



# STOMOTOLOGIYA, OPTIKA, ŞİRNİYYAT Magazalari, ENTERTAINER

data_stomotologiya1 = part1 %>% 
  filter(str_detect(pos, "STOMOTOLOGIYA"))
View(data_stomotologiya1)

#group hissesine xestexana gelir, product name de eded, ed silinmelidi

data_stomotologiya2 = part2 %>% 
  filter(str_detect(pos, "STOMOTOLOGIYA"))
View(data_stomotologiya2)

#group hissesine xestexana gelir, product name de eded, ed silinmelidi


data_stomotologiya3 = part1 %>% 
  filter(str_detect(group, "STOMOTOLOGIYA"))
View(data_stomotologiya3)

# product name temizlenmeli ve silinmelidir

data_stomotologiya4 = part2 %>% 
  filter(str_detect(group, "STOMOTOLOGIYA"))
View(data_stomotologiya4)

# product name temizlenmeli ve silinmelidir, 1701,5 manata kontrol



data_optika1 = part1 %>% 
  filter(str_detect(pos, "OPTIKA"))
View(data_optika1)

# group adlari qarisiq gelir, ekseriyyeti aptek olaraq. Pos adlari aptek optika gelir.
# qnt de onluq deyerler var

data_optika2 = part2 %>% 
  filter(str_detect(pos, "OPTIKA"))
View(data_optika2)

# group adlari qarisiq gelir, ekseriyyeti aptek olaraq. Pos adlari aptek optika gelir.
# qnt de onluq deyerler var

data_optika3 = part1 %>% 
  filter(str_detect(group, "OPTIKA"))
View(data_optika3)

# ed,eded silinmeli

data_optika4 = part2 %>% 
  filter(str_detect(group, "OPTIKA"))
View(data_optika4)

# ed,eded silinmeli



data_sirniyyat1 = part1 %>% 
  filter(str_detect(pos, "ŞİRNİYYAT Magazalari"))
View(data_sirniyyat1)

# 0 setir

data_sirniyyat2 = part2 %>% 
  filter(str_detect(pos, "ŞİRNİYYAT Magazalari"))
View(data_sirniyyat2)

# 0 setir

data_sirniyyat3 = part1 %>% 
  filter(str_detect(group, "ŞİRNİYYAT Magazalari"))
View(data_sirniyyat3)

# eded silinmeli, CFG1017, kg - menasiz setirler

data_sirniyyat4 = part2 %>% 
  filter(str_detect(group, "ŞİRNİYYAT Magazalari"))
View(data_sirniyyat4)

# eded silinmeli, kg - menasiz setirler

data_entertrainer1 = part1 %>% 
  filter(str_detect(pos, "ENTERTAINER"))
View(data_entertrainer1)

# 1 setir- group adi-magaza

data_entertrainer2 = part2 %>% 
  filter(str_detect(pos, "ENTERTAINER"))
View(data_entertrainer2)

# 1 setir pn - ENT 150 AZN 3100015011347 , up - 151

data_entertrainer3 = part1 %>% 
  filter(str_detect(group, "ENTERTAINER"))
View(data_entertrainer3)


data_entertrainer4 = part2 %>% 
  filter(str_detect(group, "ENTERTAINER"))
View(data_entertrainer4)

# DATA_MAP CLEANING


dim(data_map)
str(data_map)

colSums(is.na(data_map))

data_map |> 
  duplicated() |> 
  sum()

unique(data_map$product_type)


data_profit_margin = data_map %>%  
  filter(profit_margin < 0 | profit_margin > 1)


unique(data_map$product)

dim(data_map)
str(data_map)
colnames(data_map)
colnames(data_re)


data_map <- data_map %>%
  mutate(
    product_clean = product %>%
      str_to_upper() %>%
      str_replace_all("[^A-ZƏÖÜĞİÇ ]", " ") %>%  # simvolları sil
      str_squish()
  )













