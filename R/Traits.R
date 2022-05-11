## read public TRY traits extraction


extractNatAd <- function(spsel, TRYdata, trait){
if(sum(grepl(spsel, TRYdata$AccSpeciesName, useBytes = TRUE) &
       TRYdata$TraitID %in% trait &
       !is.na(TRYdata$TraitID) &
       (TRYdata$ErrorRisk<4 | is.na(TRYdata$ErrorRisk)))>0){
df1 <- TRYdata %>% filter(grepl(spsel, AccSpeciesName, useBytes = TRUE) &
                          TraitID %in% trait & !is.na(TraitID) &
                          (ErrorRisk<4 | is.na(ErrorRisk)))
df2 <- TRYdata %>% filter(ObservationID %in% df1$ObservationID)
#test exp
NoExp <- df2 %>% filter(DataName == "Treatment exposition") %>%
    filter(!grepl("chamber|expe|greenhouse|house|FACE|mesocosm|shade",
                  OrigValueStr,
                  ignore.case = TRUE, useBytes = TRUE))
#test size
NoJuv <- df2 %>%
    filter(DataName == "Plant developmental status / plant age / maturity / plant life stage") %>%
    filter(!grepl("seedling|sapling|juvenil|week",OrigValueStr,
                  ignore.case = TRUE, useBytes = TRUE))

if(nrow(NoExp)>0){
 dft <- df1 %>% filter(ObservationID %in% NoExp$ObservationID)
 if(sum(dft$ObservationID %in% NoJuv$ObservationID)>0){
  dft <- dft %>% filter(ObservationID %in% NoJuv$ObservationID)
  type <- "NatAd"
  }else{
  type <- "Nat"
  }
}else{
 if(sum(df1$ObservationID %in% NoJuv$ObservationID)>0){
  dft <- df1 %>% filter(ObservationID %in% NoJuv$ObservationID)
  type <- "Ad"
 }else{
  dft <- df1
  type <- "all"
 }
}
 dfres <-  dft %>% summarise(mean = mean(StdValue, na.rm = TRUE),
                             nobs = n())
res <- data.frame(mean = dfres$mean, nobs = dfres$nobs, typesel = type)
}else{
res <- data.frame(mean = NA, nobs = NA, typesel = NA)
type <- NA
}
print(spsel)
print(type)
return(res)
}

extractAll <- function(spsel, TRYdata, trait){
if(sum(grepl(spsel, TRYdata$AccSpeciesName, useBytes = TRUE) & TRYdata$TraitID %in% trait &
       !is.na(TRYdata$TraitID) & (TRYdata$ErrorRisk<3 | is.na(TRYdata$ErrorRisk)))>0){
df1 <- TRYdata %>% filter(grepl(spsel, AccSpeciesName, useBytes = TRUE) &
                          TraitID %in% trait & !is.na(TraitID) &
                          (ErrorRisk<3 | is.na(ErrorRisk)))
dfres <-  df1 %>% summarise(mean = mean(StdValue, na.rm = TRUE),
                             nobs = n())

res <- data.frame( mean = dfres$mean, nobs = dfres$nobs, typesel = "all")
}else{
res <- data.frame(mean = NA, nobs = NA, typesel = NA)
}
return(res)
}

extract_traits_public_try <- function(sps){
TRYdata1 <- data.table::fread(file.path("data", "5374.txt"),
                             header = T, sep = "\t", dec = ".",
                             quote = "", fill = TRUE, na.strings= "")
TRYdata2 <- data.table::fread(file.path("data", "5385.txt"),
                             header = T, sep = "\t", dec = ".", quote = "",
                             fill = TRUE, na.strings= "")
TRYdata <- rbind(TRYdata1, TRYdata2)
rm(TRYdata1, TRYdata2)
                                        # TRAITS to select

SLA <- c(3115) # no petiol , 3117
SLAp <- c(3116) # with petiol
SLAa <- c(3116, 3115, 3117) # with petiol

LeafArea <- c(1, 3108:3113)
LeafNmass <- 14
LeafNarea <-  50
WD <- 4
StemConduitDiam <- 281
StemConduitArea <- 170
df_SLA <- bind_rows(lapply(sps$sp, extractAll, TRYdata, SLA))
df_SLAp <- bind_rows(lapply(sps$sp, extractAll, TRYdata, SLAp))
df_SLAa <- bind_rows(lapply(sps$sp, extractAll, TRYdata, SLAa))
df_SLA$Unit <- as.vector(na.omit(unique(TRYdata$UnitName[TRYdata$TraitID %in% SLA &
                                                         !is.na(TRYdata$TraitID)])))
df_SLAp$Unit <- as.vector(na.omit(unique(TRYdata$UnitName[TRYdata$TraitID %in% SLAp &
                                                         !is.na(TRYdata$TraitID)])))
names(df_SLA) <- paste0("SLA_", names(df_SLA))
names(df_SLAp) <- paste0("SLAp_", names(df_SLAp))
names(df_SLAa) <- paste0("SLAa_", names(df_SLAa))
df_LeafArea <- bind_rows(lapply(sps$sp, extractAll, TRYdata, LeafArea))
df_LeafArea$Unit <- as.vector(na.omit(unique(TRYdata$UnitName[TRYdata$TraitID %in% LeafArea &
                                          !is.na(TRYdata$TraitID)])))
names(df_LeafArea) <- paste0("LeafArea_", names(df_LeafArea))

df_LeafNmass <- bind_rows(lapply(sps$sp, extractAll, TRYdata, LeafNmass))
df_LeafNmass$Unit <- as.vector(na.omit(unique(TRYdata$UnitName[TRYdata$TraitID %in% LeafNmass &
                                          !is.na(TRYdata$TraitID)])))
names(df_LeafNmass) <- paste0("LeafNmass_", names(df_LeafNmass))

df_LeafNarea <- bind_rows(lapply(sps$sp, extractAll, TRYdata, LeafNarea))
df_LeafNarea$Unit <- as.vector(na.omit(unique(TRYdata$UnitName[TRYdata$TraitID %in% LeafNarea &
                                          !is.na(TRYdata$TraitID)])))
names(df_LeafNarea) <- paste0("LeafNarea_", names(df_LeafNarea))

df_WD <- bind_rows(lapply(sps$sp, extractAll, TRYdata, WD))
df_WD$Unit <- as.vector(na.omit(unique(TRYdata$UnitName[TRYdata$TraitID %in% WD &
                                          !is.na(TRYdata$TraitID)])))
names(df_WD) <- paste0("WD_", names(df_WD))

df_StemConduitDiam <- bind_rows(lapply(sps$sp, extractAll, TRYdata, StemConduitDiam))
df_StemConduitDiam$Unit <-
    as.vector(na.omit(unique(TRYdata$UnitName[TRYdata$TraitID %in% StemConduitDiam &
                                          !is.na(TRYdata$TraitID)])))
names(df_StemConduitDiam) <- paste0("StemConduitDiam_", names(df_StemConduitDiam))

df_StemConduitArea <- bind_rows(lapply(sps$sp, extractAll, TRYdata, StemConduitArea))
df_StemConduitArea$Unit <-
    as.vector(na.omit(unique(TRYdata$UnitName[TRYdata$TraitID %in% StemConduitArea &
                                          !is.na(TRYdata$TraitID)])))
names(df_StemConduitArea) <- paste0("StemConduitArea_", names(df_StemConduitArea))
res <- bind_cols(df_SLA, df_SLAp, df_SLAa, df_LeafArea, df_LeafNmass, df_LeafNarea,
                 df_WD, df_StemConduitDiam, df_StemConduitArea)
res <- data.frame(sp = sps$sp, res)
return(res)
}

# extract traits data from TRY data extracted for Kunstler et al. 2016


## #READ TRAITS DATA FOR ALL EUROPEAN COUNTRY
## fun_read_TRY_traits_data <- function(sps){
 ## traits_FR <- read.csv("../../westobyLAB/workshop/data.processing2/trait.competition.workshop/output/formatted/France/traits.csv")
 ## traits_SP <- read.csv("../../westobyLAB/workshop/data.processing2/trait.competition.workshop/output/formatted/Spain/traits.csv")
 ## traits_SWiss <- read.csv("../../westobyLAB/workshop/data.processing2/trait.competition.workshop/output/formatted/Swiss/traits.csv)"
 ## traits_SWe <- read.csv("../../westobyLAB/workshop/data.processing2/trait.competition.workshop/output/formatted/Sweden/traits.csv")
 ## traits <- rbind(traits_FR, traits_SP, traits_SWiss, traits_SWe)
 ## traits$sp <- NULL
##  traits <-  traits[!duplicated(traits$Latin_name), ]
##  traits[traits$Latin_name %in% c("Betula spp."),
##         c(2:9, 14:15)] <- apply(traits[traits$Latin_name %in%
##                                                     c("Betula pubescens",
##                                                       "Betula pendula"),
##                                                     c(2:9, 14:15)], 2, mean)
##  traits[traits$Latin_name %in% c("Betula spp."), "LeafType.T"] <- "broadleaved"
##  traits$Latin_name <- as.character(traits$Latin_name)
##  traits[traits$Latin_name %in% c("Betula spp."),"Latin_name"] <-  "Betula"
##  traits <- traits[traits$Latin_name != "Betula ssp", ]
##  traits_extract <- traits[traits$Latin_name %in% sps$sp, ]
##  write.csv(traits_extract, "data/TRY_traits.csv", row.names = FALSE)
## }

plot_traits_pca <- function(){
require(ggplot2)
require(dplyr)
require(RColorBrewer)
require(ggfortify)
  traits <- read.csv("data/TRY_traits.csv",
                    stringsAsFactors=FALSE)
  traits <- traits %>% select(Latin_name, Leaf.N.mean, Seed.mass.mean,  SLA.mean,
                             Wood.density.mean, Max.height.mean) %>%
                       mutate(Seed.mass = log(Seed.mass.mean)) %>% select(-Seed.mass.mean)
  row.names(traits) <- traits$Latin_name
  data_pca <- prcomp(traits[, -1],
                     center = TRUE,
                     scale. = TRUE)
  p <- autoplot(data_pca, data = traits, colour = 'Latin_name',
              loadings = TRUE, loadings.label = TRUE, label = TRUE,
              loadings.colour = "black",
              loadings.label.colour = "black")
  data_var <- data.frame(variance = data_pca$sdev^2 / sum(data_pca$sdev^2),
                         axis = colnames(data_pca$rotation))
  g <- ggplotGrob(ggplot(data = data_var, aes(x = factor(axis), y = variance))+
                  geom_bar(stat = "identity"))
  p + annotation_custom(grob = g, xmin = -0.4,
                        xmax = -0.1, ymin = -0.4, ymax = -0.2)
ggsave(file.path("figures", "traits_pca.pdf"))
}


plot_opentraits_pca <- function(df1, df2){
require(ggplot2)
require(dplyr)
require(RColorBrewer)
require(ggfortify)

  df1s <- df1 %>% select(sp, PI50, Wood_density,
                         Whole_leaf_size_cm2, SLA, Narea, Nmass)
  row.names(df1s) <- df1s$sp
  data_pca <- prcomp(na.exclude(df1s[, -1]),
                     center = TRUE,
                     scale. = TRUE)
  p <- autoplot(data_pca, data = na.exclude(df1s), colour = 'sp',
              loadings = TRUE, loadings.label = TRUE, label = TRUE,
              loadings.colour = "black",
              loadings.label.colour = "black")
  data_var <- data.frame(variance = data_pca$sdev^2 / sum(data_pca$sdev^2),
                         axis = colnames(data_pca$rotation))
  g <- ggplotGrob(ggplot(data = data_var, aes(x = factor(axis), y = variance))+
                  geom_bar(stat = "identity"))
  p + annotation_custom(grob = g, xmin = -0.4,
                        xmax = -0.1, ymin = -0.4, ymax = -0.2)
ggsave(file.path("figures", "opentraits_pca.pdf"))

df2s <- df2 %>% select(sp, SLA_mean, LeafArea_mean, LeafNmass_mean, LeafNarea_mean,
                       StemConduitDiam_mean, WD_mean)
  row.names(df2s) <- df2s$sp
  data_pca <- prcomp(na.exclude(df2s[, -1]),
                     center = TRUE,
                     scale. = TRUE)
  p <- autoplot(data_pca, data = na.exclude(df2s), colour = 'sp',
              loadings = TRUE, loadings.label = TRUE, label = TRUE,
              loadings.colour = "black",
              loadings.label.colour = "black")
  data_var <- data.frame(variance = data_pca$sdev^2 / sum(data_pca$sdev^2),
                         axis = colnames(data_pca$rotation))
  g <- ggplotGrob(ggplot(data = data_var, aes(x = factor(axis), y = variance))+
                  geom_bar(stat = "identity"))
  p + annotation_custom(grob = g, xmin = -0.4,
                        xmax = -0.1, ymin = -0.4, ymax = -0.2)
ggsave(file.path("figures", "openTRYtraits_pca.pdf"))

}


data_traits_pca <- function(sps){
  traits <- read.csv("data/TRY_traits.csv",
                    stringsAsFactors=FALSE)
  traits <- traits %>% select(Latin_name, Leaf.N.mean, Seed.mass.mean,  SLA.mean,
                             Wood.density.mean, Max.height.mean) %>%
                       mutate(Seed.mass = log(Seed.mass.mean)) %>% select(-Seed.mass.mean)
  row.names(traits) <- traits$Latin_name
  return(traits[sps$sp, ])
}

##############################
### Extract public traits

## function to extract with genus mean

get_genus <- function(sp){
  genus <- sub(" .*", "", sp)
  return(genus)
}


extract_one_trait <- function(var, spsel2, df){
   df <- as.data.frame(df)
   res <- data.frame(NA, NA)
   names(res) <- c(var, paste0(var,"_GenusM"))
  if(sum(grepl(spsel2, df$Binomial[!is.na(df[[var]])])) >0){
    res[[var]] <- mean(df[grepl(spsel2, df$Binomial), var], na.rm = TRUE)
    res[[2]] <- "NO"
  }else{
    genus_sel<- unique(get_genus(spsel2))
    genus<- get_genus(df$Binomial[!is.na(df[[var]])])
    if(sum(grepl(genus_sel, genus)) >0){
    res[[var]] <- NA#mean(df[!is.na(df[[var]]), ][grepl(genus_sel, genus), var], na.rm = TRUE)
    res[[2]] <- "YES"
    }else{
    res[[var]] <- NA
    res[[2]] <- NA
    }
  }
return(res)
}


extract_traits <- function(spsel, df, vars){
if (spsel == "Betula") {
    spsel2 <- "Betula pubescens|Betula pendula"
 }else{
   if (spsel == "Pinus uncinata") {
    spsel2 <- "Pinus uncinata|Pinus mugo"
   }else{
    spsel2 <- spsel
   }
 }
res <- bind_cols(lapply(vars, extract_one_trait, spsel2, df))
res$sp <- spsel
res <- select(res, sp, everything())
 return(res)
}




## function extract all data
extract_public_trait <- function(sps, wright2004, wright2017, maire, chave, zanne, choat){
    spvec <- sps$sp
extract_wright2004 <- bind_rows(lapply(spvec,extract_traits, wright2004,
                                      vars = c("leaflifespan", "lma", "n.mass",
                                               "n.area", "p.mass", "p.area")))
extract_maire <- bind_rows(lapply(spvec,extract_traits, maire,
                             vars = c("SLA", "Nmass","Narea",
                                      "Pmass", "Parea")))
# MERGE BOTH
extract_maire$SLA <- extract_maire$SLA/10 # convert in m2 per kg
extract_maire$Nmass <- extract_maire$Nmass*10 # convert in mg per g
extract_maire$Narea <- extract_maire$Narea/1000 # convert in kg per m2
extract_wright2004$SLA <- (1/(extract_wright2004$lma*1000)*10000)/10 # convert in m2 per kg
extract_wright2004$Narea <- extract_wright2004$n.area # in kg per m2
extract_wright2004$Nmass <- extract_wright2004$n.mass*1000/100 # convert in mg per g
extract_wright2004$SLA_GenusM<- extract_wright2004$lma_GenusM
extract_wright2004$Narea_GenusM <- extract_wright2004$n.area_GenusM
extract_wright2004$Nmass_GenusM <- extract_wright2004$n.mass_GenusM
# get missing data in Wright2004 from Maire
data_leaf <- extract_wright2004[, c("sp", "SLA", "SLA_GenusM",
                               "Narea", "Narea_GenusM",
                               "Nmass", "Nmass_GenusM")]
data_leaf_W<- extract_maire[, c("sp", "SLA", "SLA_GenusM",
                                     "Narea", "Narea_GenusM",
                                     "Nmass", "Nmass_GenusM")]
names(data_leaf_W) <- paste0(names(data_leaf_W), "_W")
data_m <- left_join(data_leaf, data_leaf_W, by = c("sp" = "sp_W"))
#get species level from both data keeping Wright as more data
data_m <- data_m %>% mutate(SLA2 = case_when(SLA_GenusM == "NO" & !is.na(SLA_GenusM) ~ SLA,
                                             SLA_GenusM == "YES" & !is.na(SLA_GenusM) &
                                             SLA_GenusM_W== "NO" & !is.na(SLA_GenusM_W) ~ SLA_W,
                                             SLA_GenusM == "YES" & !is.na(SLA_GenusM) &
                                             SLA_GenusM_W== "YES" & !is.na(SLA_GenusM_W) ~ SLA,
                                             is.na(SLA_GenusM) & !is.na(SLA_GenusM_W) ~ SLA_W,
                                             is.na(SLA_GenusM) & is.na(SLA_GenusM_W) ~ SLA),
                            SLA2_GenusM= case_when(SLA_GenusM == "NO" & !is.na(SLA_GenusM) ~ SLA_GenusM,
                                             SLA_GenusM == "YES" & !is.na(SLA_GenusM) &
                                             SLA_GenusM_W== "NO" & !is.na(SLA_GenusM_W) ~ SLA_GenusM_W,
                                             SLA_GenusM == "YES" & !is.na(SLA_GenusM) &
                                             SLA_GenusM_W== "YES" & !is.na(SLA_GenusM_W) ~ SLA_GenusM,
                                             is.na(SLA_GenusM) & !is.na(SLA_GenusM_W) ~ SLA_GenusM_W,
                                             is.na(SLA_GenusM) & is.na(SLA_GenusM_W) ~ SLA_GenusM),
                            Narea2 = case_when(Narea_GenusM == "NO" & !is.na(Narea_GenusM) ~ Narea,
                                             Narea_GenusM == "YES" & !is.na(Narea_GenusM) &
                                             Narea_GenusM_W== "NO" & !is.na(Narea_GenusM_W) ~ Narea_W,
                                             Narea_GenusM == "YES" & !is.na(Narea_GenusM) &
                                             Narea_GenusM_W== "YES" & !is.na(Narea_GenusM_W) ~ Narea,
                                             is.na(Narea_GenusM) & !is.na(Narea_GenusM_W) ~ Narea_W,
                                             is.na(Narea_GenusM) & is.na(Narea_GenusM_W) ~ Narea),
                            Narea2_GenusM= case_when(Narea_GenusM == "NO" & !is.na(Narea_GenusM) ~ Narea_GenusM,
                                             Narea_GenusM == "YES" & !is.na(Narea_GenusM) &
                                             Narea_GenusM_W== "NO" & !is.na(Narea_GenusM_W) ~ Narea_GenusM_W,
                                             Narea_GenusM == "YES" & !is.na(Narea_GenusM) &
                                             Narea_GenusM_W== "YES" & !is.na(Narea_GenusM_W) ~ Narea_GenusM,
                                             is.na(Narea_GenusM) & !is.na(Narea_GenusM_W) ~ Narea_GenusM_W,
                                             is.na(Narea_GenusM) & is.na(Narea_GenusM_W) ~ Narea_GenusM),
                            Nmass2 = case_when(Nmass_GenusM == "NO" & !is.na(Nmass_GenusM) ~ Nmass,
                                             Nmass_GenusM == "YES" & !is.na(Nmass_GenusM) &
                                             Nmass_GenusM_W== "NO" & !is.na(Nmass_GenusM_W) ~ Nmass_W,
                                             Nmass_GenusM == "YES" & !is.na(Nmass_GenusM) &
                                             Nmass_GenusM_W== "YES" & !is.na(Nmass_GenusM_W) ~ Nmass,
                                             is.na(Nmass_GenusM) & !is.na(Nmass_GenusM_W) ~ Nmass_W,
                                             is.na(Nmass_GenusM) & is.na(Nmass_GenusM_W) ~ Nmass),
                            Nmass2_GenusM= case_when(Nmass_GenusM == "NO" & !is.na(Nmass_GenusM) ~ Nmass_GenusM,
                                             Nmass_GenusM == "YES" & !is.na(Nmass_GenusM) &
                                             Nmass_GenusM_W== "NO" & !is.na(Nmass_GenusM_W) ~ Nmass_GenusM_W,
                                             Nmass_GenusM == "YES" & !is.na(Nmass_GenusM) &
                                             Nmass_GenusM_W== "YES" & !is.na(Nmass_GenusM_W) ~ Nmass_GenusM,
                                             is.na(Nmass_GenusM) & !is.na(Nmass_GenusM_W) ~ Nmass_GenusM_W,
                                             is.na(Nmass_GenusM) & is.na(Nmass_GenusM_W) ~ Nmass_GenusM))
leaf <- data_m[, c("sp", "SLA2", "Narea2", "Nmass2", "SLA2_GenusM", "Narea2_GenusM", "Nmass2_GenusM")]
names(leaf) <- c("sp", "SLA", "Narea", "Nmass", "SLA_GenusM", "Narea_GenusM", "Nmass_GenusM")
## Pinus uncinata Handa et al. Ecology 2005 SLA cm2 / g (41.3 41.6 )/2 = 41.45 => 41.45/10 m2 per kg;
## N mass (1.18 +1.29)/2*1000/100 = 12.35 to convert in mg per g
## Handa, I. Tanya, Christian Körner, and Stephan Hättenschwiler 2005A TEST OF THE TREELINE CARBON LIMITATION HYPOTHESIS BY IN SITU CO 2 ENRICHMENT AND DEFOLIATION. Ecology 86(5): 1288–1300.
leaf[leaf$sp == "Pinus uncinata", c("SLA", "Nmass")] <- c(41.45/10, 12.35)
leaf[leaf$sp == "Pinus uncinata", c("SLA_GenusM", "Nmass_GenusM")] <- c("NO", "NO")
## Juniperus thurifera Nmass (percentage) 0.8102*10 = 8.102
# from Nowak-Dyjeta, Kinga, M. J. Giertych, P. Thomas, and G. Iszkuło 2017Males and Females of Juniperus Communis L. and Taxus Baccata L. Show Different Seasonal Patterns of Nitrogen and Carbon Content in Needles. Acta Physiologiae Plantarum 39(8). http://link.springer.com/10.1007/s11738-017-2489-3, accessed January 30, 2019.
# From PEnuelas 1999 Nmass in perc (extracted from figure 3)
 ## mean(c(1.7890910836894416,
 ##   1.5984539299947185,
 ##   1.3009061040270529,
 ##   1.3995308288005268,
 ##   1.600956176391909)) = 1.537788
## Nmass (0.8102+1.53)/2*10 = 11.7
leaf[leaf$sp == "Juniperus thurifera", c("Nmass")] <- c(14.2)
leaf[leaf$sp == "Juniperus thurifera", c("Nmass_GenusM")] <- c("NO")
# (6.57 +6.82 +7.69 )/3 Porte et al 2000 Ann. For. Sci.
leaf[leaf$sp == "Pinus pinaster", "SLA"] <- 7.026
leaf[leaf$sp == "Pinus pinaster", "SLA_GenusM"] <- "NO"
## ## compute Narea from Nmass and SLA (m2 per kg)
## leaf$Narea[is.na(leaf$Narea) &
##            !is.na(leaf$SLA) &
##            !is.na(leaf$Nmass)] <- leaf$Nmass[is.na(leaf$Narea) &
##                                              !is.na(leaf$SLA) &
##                                              !is.na(leaf$Nmass)]/(1000 *
##                                                leaf$SLA[is.na(leaf$Narea) &
##                                                         !is.na(leaf$SLA) &
##                                                         !is.na(leaf$Nmass)])
# Leaf size
extract_wright2017 <- bind_rows(lapply(spvec,extract_traits, wright2017,
                             vars = c("Leaf_size_cm2",
                                      "Whole_leaf_size_cm2")))
# wood density
extract_chave <- bind_rows(lapply(spvec,extract_traits, as.data.frame(chave),
                             vars = c("Wood_density")))
# Quercus pubsecens  0.6426398
# Quercus faginea 0.8259269
#Castro-Diez, P., J. P. Puyravaud, J. H. C. Cornelissen, and P. Villar-Salvador. 1998. Stem anatomy and relative growth rate in seedlings of a wide range of woody plant species and types. Oecologia 116:57-66.
extract_chave[extract_chave$sp == "Quercus pubescens", "Wood_density"] <- 0.6426398
extract_chave[extract_chave$sp == "Quercus faginea", "Wood_density"] <- 0.8259269
extract_chave[extract_chave$sp == "Quercus pubescens", "Wood_density_GenusM"] <- "NO"
extract_chave[extract_chave$sp == "Quercus faginea", "Wood_density_GenusM"] <- "NO"
# vessel size
extract_zanne <- bind_rows(lapply(spvec,extract_traits, zanne,
                             vars = c("A_mm_2", "F_mm_2_mm_2",
                                      "N_mm_2", "S_mm_4")))
# PI50
extract_choat <- bind_rows(lapply(spvec,extract_traits, choat,
                             vars = c("PI50", "PI88", "Psi_min_midday", "Psi_min",
                                      "Psi_50_safety_margin", "Psi_88_safety_margin")))
extract_choat[extract_choat$GenusM != "NO", -c(1,8)]<-  NA
# merge all
df <- cbind(leaf[, c("sp", "SLA", "Narea", "Nmass", "SLA_GenusM", "Narea_GenusM", "Nmass_GenusM")],
            extract_wright2017[, -1],
            extract_chave[, -1],
            extract_zanne[, -1],
            extract_choat[, -1])

return(df)
}

## compare three source of traits

format_open_try_choat <- function(TRYpublic, open_traits){

    data <- TRYpublic[, c("sp", "SLA_mean", "LeafArea_mean",
                  "LeafNmass_mean", "LeafNarea_mean","WD_mean")]
    names(data) <- c("sp", "SLA", "Leaf_size_cm2", "Nmass", "Narea", "Wood_density")
    data$Narea <- data$Narea/1000 #(convert g m-2 in kg m-2)
    data$SLA[is.na(data$SLA)] <- open_traits$SLA[is.na(data$SLA)]
    data$Nmass[is.na(data$Nmass)] <- open_traits$Nmass[is.na(data$Nmass)]
    data$Narea[is.na(data$Narea)] <- open_traits$Narea[is.na(data$Narea)]
    data$PI50 <- open_traits$PI50
   return(data)
}


plot_compare_traits <- function(df1, df2, df3){
 pdf("figures/compare_traits.pdf", width = 14, height = 10)
 par(mfrow = c(1,2))
 plot(df1$Leaf.N.mean, df2$LeafNmass_mean,
      xlab = "Leaf N Perc TRY Kunstler 2016",
      ylab = "Leaf N Perc TRY public 2018",
      pch= "")
 text(df1$Leaf.N.mean, df2$LeafNmass_mean, labels = df1$Latin_name)
 abline(a = 0, b = 1)
 plot(df1$Leaf.N.mean, df3$Nmass,
      xlab = "Leaf N Perc TRY Kunstler 2016",
      ylab = "Leaf N Wright and Maire",
      pch= "")
 abline(a = 0, b = 1)
 text(df1$Leaf.N.mean, df3$Nmass, labels = df1$Latin_name)
 par(mfrow = c(1,1))
 plot(df2$LeafNarea_mean, df3$Narea,
      xlab = "Leaf N area TRY public 2018",
      ylab = "Leaf N area Wright and Maire",
      pch= "")
 text(df2$LeafNarea_mean, df3$Narea, labels = df1$Latin_name)
 abline(a = 0, b = 1)


 par(mfrow = c(1,2))
 plot(df1$SLA.mean, df2$SLA_mean,
      xlab = "SLA TRY Kunstler 2016",
      ylab = "SLA TRY public 2018",
      pch= "")
 abline(a = 0, b = 1)
 text(df1$SLA.mean, df2$SLA_mean, labels = df1$Latin_name)
 plot(df1$SLA.mean, df3$SLA,
      xlab = "SLA TRY Kunstler 2016",
      ylab = "SLA Wright and Maire",
      pch= "")
 text(df1$SLA.mean, df3$SLA, labels = df1$Latin_name)
 abline(a = 0, b = 1)


 par(mfrow = c(1,2))
 plot(df1$Wood.density.mean, df2$WD_mean,
      xlab = "WD TRY Kunstler 2016",
      ylab = "WD TRY public 2018",
      pch= "")
 abline(a = 0, b = 1)
 text(df1$Wood.density.mean, df2$WD_mean, labels = df1$Latin_name)
 plot(df1$Wood.density.mean, df3$Wood_density,
      xlab = "WD TRY Kunstler 2016",
      ylab = "WD Chave", pch= "")
 abline(a = 0, b = 1)
 text(df1$Wood.density.mean, df3$Wood_density, labels = df1$Latin_name)

 plot(df2$LeafArea_mean/100, df3$Leaf_size_cm2,
      xlab = "Leaf area TRY public 2018",
      ylab = "Leaf area Wright 2017", pch= "")
 abline(a = 0, b = 1)
 text(df2$LeafArea_mean/100, df3$Leaf_size_cm2, labels = df1$Latin_name)

 dev.off()
}
