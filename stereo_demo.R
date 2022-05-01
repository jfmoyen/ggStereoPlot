
#### Demo usage ####

source("stereoplots.R")

foo <- tibble(Az=runif(5,0,360),Dp=runif(5,0,90))

foo %>%
  ggplot() + base_stereo +
  geom_point(aes(x=Az,y=Dp),size=4)

foo %>%
  ggplot() + base_stereo +
  geom_point(aes(x=Az,y=Dp),size=4,stat=StatStereoPole) ## = convention="strike-rhr"

foo %>%
  ggplot() + base_stereo +
  geom_point(aes(x=Az,y=Dp),size=4,stat=StatStereoPole,convention="dipdir")

foo %>%
  ggplot() + base_stereo +
  geom_path(aes(x=Az,y=Dp),stat=StatStereoPlane)

stop("Proceed beyond this point at own risk !")
### Below does not always work, connection is temepramental... handle with care !

##### Better data ####
library(readxl)
donnees_L2 <- read_excel("C:/Users/moje4671/Desktop/donnees_L2.xlsx")

library(googlesheets4)
library(googledrive)
#drive_auth()

#read_sheet("https://docs.google.com/spreadsheets/d/1gmc7tVfUTcM412PRGDYv0H2-DFTAm57MulD4Y410Al8")

donnees_L2 <- read_sheet(drive_get("Mesures structurales L2"))

donnees_L2 %>% 
  filter(Operateur != "AT") %>%
  mutate(Nature=case_when(
    Nature=="Faille et stries" ~ "Faille",
    TRUE~Nature
  )) %>%
  mutate(TP=RakeToTrend(P_strike,P_dip,L_pitch)) %>% unnest(TP) %>%
  ggplot() +
  base_stereo +
  geom_path(aes(x=P_dipdirection,y=P_dip,color=Nature),stat=StatStereoPlane,convention="dipdir")+
  geom_point(aes(x=trend,y=plunge,color=Nature))+
  facet_wrap(~Site)


donnees_L2 %>% filter(Nature=="Strati") %>%
  ggplot()+base_stereo+
  #geom_point(aes(x=P_strike_rhr,y=P_dip,color=Site),stat=StatStereoPole)
  geom_path(aes(x=P_strike_rhr,y=P_dip,color=Site),stat=StatStereoPlane)
