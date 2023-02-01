###########################################################################################################
#SF-Feb-22-091
#Historical church competition predicts religiosity in the 2000s:the case of the Netherlands
#Social Forces
#Brik Tymofii
#tbrik@kse.org.ua
#
#DATA SOURCES
#Europeand Social Survey, the Netherlands, 2002-2008
#Centraal Bureau voor de Statistiek, the Netherlands
#Historical International Standardized Community Indicators, the Netherlands. Utrecht University.


                                            ########
                                            #STEP 1#
                                            ########
#Install all the packages for the data analysis and visualization
library(car) 
library(classInt)
library(foreign)
library(ggplot2)
library(lattice)
library(lme4) 
library(mapproj)
library(maps)
library(mapdata)
library(maptools) 
library(multilevel)
library(plyr)
library(psych)
library(RColorBrewer)
library(reshape)
library(rgeos)
library(stringr)
#
#Set up the working directory
setwd("/Users/tymofiibrik/Documents/papers/Working papers/Dissertation/Historical roots of secularization")                                           
#setwd("") #your path
#
#Open the data
#the historical data provided by Utrecht University team
hinl<-read.csv("hinl.utrecht.csv")
#the ESS data for 2002-2008 + the data about gdp/migration in 2000 
spss<-as.data.frame(read.spss("ess_merged_final analysis.sav"))
#Dutch statistical office. HHI in 2008
hhi_cbs<-read.csv("cbs_hhi.csv")
#                                                                             
#Merge Zuidwest-Drenthe with Zuidoost-Drenthe due to the limited data (discussed in the paper)
levels(hinl$minecorop)[levels(hinl$minecorop)=="Zuidwest-Drenthe"] <- "Zuidoost-Drenthe"
#                                    
#Herfindahl Index for 2002-2008 by COROPS
romancath_sum2<-tapply(spss$romancath, spss$minecorop, sum, na.rm=TRUE)
protestant_sum2<-tapply(spss$protestant, spss$minecorop, sum, na.rm=TRUE)
easternorh_sum2<-tapply(spss$easternorh, spss$minecorop, sum, na.rm=TRUE)
christian_sum2<-tapply(spss$christian, spss$minecorop, sum, na.rm=TRUE)
jewish_sum2<-tapply(spss$jewish, spss$minecorop, sum, na.rm=TRUE)
islamic_sum2<-tapply(spss$islamic, spss$minecorop, sum, na.rm=TRUE)
eastern_sum2<-tapply(spss$eastern, spss$minecorop, sum, na.rm=TRUE)
other_sum2<-tapply(spss$other, spss$minecorop, sum, na.rm=TRUE)
#
hhi_ess<- as.data.frame(cbind(romancath_sum2, protestant_sum2, easternorh_sum2,
                              christian_sum2, jewish_sum2, islamic_sum2,
                              eastern_sum2,  other_sum2))
#
hhi_ess$total_r<- (hhi_ess$romancath_sum2+
                     hhi_ess$protestant_sum2+
                     hhi_ess$easternorh_sum2+
                     hhi_ess$christian_sum2+
                     hhi_ess$jewish_sum2+
                     hhi_ess$islamic_sum2+
                     hhi_ess$eastern_sum2+
                     hhi_ess$other_sum2)
#
#Final HHI index where 0 means competition and 1 means concentration
hhi_ess$hhi_ess_r<- ( ((hhi_ess$romancath_sum2/hhi_ess$total_r)^2) + 
                         ((hhi_ess$protestant_sum2/hhi_ess$total_r)^2) + 
                         ((hhi_ess$easternorh_sum2/hhi_ess$total_r)^2) + 
                         ((hhi_ess$christian_sum2/hhi_ess$total_r)^2) + 
                         ((hhi_ess$jewish_sum2/hhi_ess$total_r)^2) + 
                         ((hhi_ess$islamic_sum2/hhi_ess$total_r)^2) + 
                         ((hhi_ess$eastern_sum2/hhi_ess$total_r)^2) + 
                         ((hhi_ess$other_sum2/hhi_ess$total_r)^2) ) 
#                    
hhi_ess$minecorop<-row.names(hhi_ess)
############################################################################################
#Making a dependent variable
#SECULARIZATION
#1 if a person does not belong to any church ("NO")                                      
table(spss$rlgblg)
spss$nod[spss$rlgblg=="No"]<-1
spss$nod[spss$rlgblg=="Yes"]<-0
spss$nod[spss$rlgblg=="Refusal"]<-0
spss$nod[spss$rlgblg=="Don't know"]<-0
spss$nod[spss$rlgblg=="No answer"]<-0
#                                           
#Some other variables                                            
#A new dummy variable "Was born in the Netherlands"
#1- was born, 0 - else
spss$born[spss$brncntr=="Yes"]<-1
spss$born[spss$brncntr=="No"]<-0
spss$born[spss$brncntr=="Refusal"]<-0
spss$born[spss$brncntr=="Don't know"]<-0
spss$born[spss$brncntr=="No answer"]<-0
#
#Scale some of the variables to get better interpretation in the models
#Log variable for gdp.
spss$netm2000scale<-scale(spss$netm2000, center = TRUE, scale = TRUE)
spss$gdp2000scale<-scale(spss$gdp2000, center = TRUE, scale = TRUE)
spss$gdplog<-log(spss$gdp2000)
#
#Making contextual histroical variables (trains, education, predominant religion)
hinl$one<-1
municipal<-tapply(hinl$one, hinl$minecorop, sum, na.rm=TRUE)
train_st<-tapply(hinl$train_1909, hinl$minecorop, sum, na.rm=TRUE)
stud<-tapply(hinl$studsec_1909, hinl$minecorop, sum, na.rm=TRUE)
popul<-tapply(hinl$popul_1909, hinl$minecorop, sum, na.rm=TRUE)
sumall<-tapply(hinl$sumall, hinl$minecorop, sum, na.rm=TRUE)
#
cath<-tapply(hinl$RoomsKatholie, hinl$minecorop, sum, na.rm=TRUE)
prot_both<-tapply(hinl$Prot_both, hinl$minecorop, sum, na.rm=TRUE)
#                                         
development<-as.data.frame(cbind(municipal,train_st, stud, sumall, popul, #Development" as for "socio-economic development
                                 cath, prot_both))                              
#
development$popul_perhandred<- development$popul/100
development$educ_per<- development$stud/development$popul_perhandred
development$stud_share<- development$stud/development$popul
development$train_share<- development$train_st/development$municipal
development$cath<- development$cath/development$sumall
development$prot_both<- development$prot_both/development$sumall
development$minecorop<- as.factor(row.names(development))
#############################################################################################
#Herfindahl index for 1909
hhi1909_2<-aggregate(hinl$hhi, by=list(hinl$minecorop), FUN=mean, na.rm=T)
names(hhi1909_2)<-c("minecorop", "hhi_1909_2")
####################################################################################################
                                            #FINAL DATA
                                            #SPPS + historical data
development$minecorop<-str_trim(development$minecorop)
spss$minecorop<-str_trim(spss$minecorop)
hhi1909_2$minecorop<-str_trim(hhi1909_2$minecorop)
hhi_ess$minecorop<-str_trim(hhi_ess$minecorop)
#
spss_developments<-merge(spss, development, by="minecorop")
spss_developments<-merge(spss_developments, hhi_ess, by="minecorop")                                            
spss_developments<-merge(spss_developments, hhi1909_2, by="minecorop")
#
#recode missing values in ESS for age to missings
spss_developments$agea[spss_developments$agea==999] <- NA
#
#a new dummy variable for a predominant religion
#a threshold is 60%
spss_developments$mostly_protestant<-0
spss_developments$mostly_protestant[which(spss_developments$prot_both>=0.60)]<-1
spss_developments$mostly_cath<-0
spss_developments$mostly_cath[which(spss_developments$cath>=0.60)]<-1
#
#Making dummy variables for the models. Trains and Education
#The threshold is the average value                                            
spss_developments$train_d<-0  
spss_developments$train_d[spss_developments$train_share>0.30]<-1
#
spss_developments$educ_d<-0  
spss_developments$educ_d[spss_developments$educ_per>0.83]<-1   
#
names(spss_developments)
#

#####################################################################################################
                                            
                                        ########
                                        #STEP 2#
                                        ########

#Some descriptive statitstics for the analysis
#Table 1. Descriptive statistics, page 15 in the article
                                            
#Being without any denomination                                            
summary(spss_developments$nod)
#
#summary(spss_developments$)
#sd(spss_developments$)
#
#Number of mostly Protestant regions 
summary(spss_developments$mostly_protestant)
table(spss_developments$mostly_protestant)                                      
#
#trains in 1909 per NUTS unit                                            
table(spss_developments$minecorop,spss_developments$train_share)
summary(spss_developments$train_share)
sd(spss_developments$train_share)
summary(spss_developments$train_d)    
#
                                            
#Educational expansion in 1909                                             
summary(spss_developments$educ_per)
sd(spss_developments$educ_per)
summary(spss_developments$educ_d)                                                
#
#GDP in a region in 2000
summary(spss_developments$gdp2000)
sd(spss_developments$gdp2000)
#
#Net migration in 2000 
summary(spss_developments$netm2000)
sd(spss_developments$netm2000)
#
#Age 
summary(spss_developments$agea)
sd(spss_developments$agea, na.rm=TRUE)
#
#Male 
summary(spss_developments$male)
sd(spss_developments$male)
#
#ISEI 
summary(spss_developments$isei)
sd(spss_developments$isei, na.rm=TRUE)
length(spss_developments$isei)
#
#Born in the country 
summary(spss_developments$born)
sd(spss_developments$born)
####################################################################################################
                                              ########
                                              #STEP 3#
                                              ########
                                            
                                              #MODELS#                                        
#Model 0 is not in the paper
Model0 <- glmer( nod  ~ hhi_1909_2*mostly_protestant+
               (1| minecorop ), 
               data = spss_developments, family = "binomial")
summary(Model0)                                  
#
Model1 <- glmer( nod  ~ hhi_1909_2+mostly_protestant+
                       agea+male+isei+born+
                       (1| minecorop ), 
                       data = spss_developments, family = "binomial")
summary(Model1)
0.3198/sqrt(39)
#
#The same model with a linear link function (linear binary specification).                                      
Model1.test <- lmer( nod  ~ hhi_1909_2+mostly_protestant+
                       agea+male+isei+born+
                       (1| minecorop ), 
                       data = spss_developments)
summary(Model1.test)                                         
Anova(Model1.test)    
AIC(Model1.test)  
0.0768/sqrt(39)
0.005/(0.005+0.225091)*100      
#
######################################################################################################
Model2<- glmer( nod  ~ hhi_1909_2+mostly_protestant+
                       train_d+
                       educ_d+
                       agea+male+isei+born+
                       (1| minecorop ), data = spss_developments, family = "binomial")
summary(Model2)
AIC(Model2)
0.2907/sqrt(39)
0.09002/0.04
#The same model with a linear link function (linear binary specification).    
Model2.test<- lmer( nod  ~ hhi_1909_2+mostly_protestant+
                      train_d+
                      educ_d+
                agea+male+isei+born+
                (1| minecorop ), data = spss_developments)
summary(Model2.test)
Anova(Model2.test)
AIC(Model2.test)
0.07/sqrt(39)
####################################################################################
Model3 <- glmer( nod  ~  hhi_1909_2+mostly_protestant+
                       train_d+
                       educ_d+
                       gdp2000scale+netm2000scale+
                       agea+male+isei+born+
                       (1| minecorop ), 
                        data = spss_developments, family = "binomial")
summary(Model3)
AIC(Model3)
0.2892/sqrt(39)
#The same model with a linear link function (linear binary specification).    
Model3.test <- lmer( nod  ~ hhi_1909_2+mostly_protestant+
                train_d+
                educ_d+
                gdp2000scale+netm2000scale+#muslims_share+
                agea+male+isei+born+
                (1| minecorop ), 
                data = spss_developments)
summary(Model3.test)  
Anova(Model3.test)
AIC(Model3.test)
#                                                                                    
Model4 <- glmer(nod  ~ hhi_1909_2*mostly_protestant+
                       train_d*mostly_protestant+
                       educ_d*mostly_protestant+
                       gdp2000scale+
                       netm2000scale+
                       agea+male+isei+born+
                       (1| minecorop ), data = spss_developments, family = "binomial")

summary(Model4) 
AIC(Model4)                                            
0.06/sqrt(39)     
#The same model with a linear link function (linear binary specification).    
Model4.test <- lmer( nod  ~ hhi_1909_2*mostly_protestant+
                 hhi_1909_2*train_d+
                 hhi_1909_2*educ_d+
                 gdp2000scale+
                 netm2000scale+
                 agea+male+isei+born+
                 (1| minecorop ), 
                 data = spss_developments)
summary(Model4.test)
Anova(Model4.test)
#                                            
summary(Model4.test)
AIC(Model4.test)
Anova(Model4.test)
                                            #PLOT PREDICTED PROB-S + INTERACTIONS
# temporary data
tmpdat <- spss_developments[, c("hhi_1909_2", "mostly_protestant", "train_d",
                                "educ_d", "gdp2000scale", "netm2000scale",
                                "agea", "male", "isei", "born", "minecorop" )]
# calculate predicted probabilities and store in a list
spss_developments$mostly_protestant<-as.factor(spss_developments$mostly_protestant)
tmpdat$mostly_protestant<-as.factor(tmpdat$mostly_protestant)   
spss_developments$train_d<-as.factor(spss_developments$train_d)
tmpdat$train_d<-as.factor(tmpdat$train_d) 
spss_developments$educ_d<-as.factor(spss_developments$educ_d)
tmpdat$educ_d<-as.factor(tmpdat$educ_d) 
# calculate predicted probabilities and store in a list
jvalues <- with(spss_developments, seq(from = min(hhi_1909_2), to = max(hhi_1909_2), length.out = 100))
#                                            
biprobs <-list()
biprobs <- lapply(levels(spss_developments$mostly_protestant), function(stage) {
tmpdat$mostly_protestant[] <- stage
lapply(jvalues, function(j) {
tmpdat$hhi_1909_2 <- j
predict(Model4, newdata = tmpdat, type = "response")
})
}) 
#        
# get means and quartiles for all jvalues for each level of CancerStage
plotdat2 <- lapply(biprobs, function(X) {
  temp <- t(sapply(X, function(x) {
  c(M=mean(x, na.rm=T), quantile(x, c(.25, .75), na.rm=T))
  }))
  temp <- as.data.frame(cbind(temp, jvalues))
  colnames(temp) <- c("PredictedProbability_Secular", "Lower", "Upper", "Religious_Monopolization")
  return(temp)
})
# collapse to one data frame
plotdat2.2 <- do.call(rbind, plotdat2)                                          
#
plotdat2.2$Predominantly.Protestant.in.1909 <- factor(rep(levels(spss_developments$mostly_protestant), each = length(jvalues)))                                            
head(plotdat2.2)
# graph it
p<-ggplot(plotdat2.2, aes(x = Religious_Monopolization, y = PredictedProbability_Secular)) +
geom_line(aes(colour = Predominantly.Protestant.in.1909, linetype = Predominantly.Protestant.in.1909), size = 2) +
ylim(c(0, 1)) + 
xlab("Religious Monopolization (HHI) ") +
ylab("Predicted Probabilities") +
theme(legend.title = element_text(face = "italic"))#+
#
p
#
jpeg("Figure4.jpeg", width=800, height=600)
p
dev.off()
#####################################################################################################
                                              ########
                                              #STEP 4#
                                          #MAKING SOME MAPS#
                                          ##################
#Download the data
Regions2 <- readShapePoly("NLD_adm2.shp")
sec1909 <- read.csv("secul1909.csv", header=T, sep=",")
#
map_data <- as.data.frame(Regions2@data)
map_data$bla<-c(1:dim(map_data)[1])
#
gemeente<-fortify(Regions2, region = "NAME_2")
cnames <-aggregate(cbind(long, lat) ~ id, data = gemeente,
                   FUN = function(x) mean(range(x)))
#
head(sec1909)
sec1909$norel
p1<-ggplot() + geom_map(data = gemeente, aes(map_id = id),
                        map = gemeente, color = "black", fill="white") + 
  expand_limits(x = gemeente$long, y = gemeente$lat) +
  coord_map("polyconic")+
  geom_point(aes(longitude..dec.degrees., latitude..dec.degrees.,
                 fill = NULL,group = NULL), size = sec1909$norel/3,data=sec1909, color="red")+
  theme(axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
#
p2<-ggplot() + geom_map(data = gemeente, aes(map_id = id),
                        map = gemeente, color = "black", fill="white") + 
  expand_limits(x = gemeente$long, y = gemeente$lat) +
  coord_map("polyconic")+
  geom_point(aes(longitude..dec.degrees., latitude..dec.degrees.,
                 fill = NULL,group = NULL), size = 1.5, data=sec1909, 
             color="red")+
  theme(axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
#
grid.arrange(p2, p1, ncol = 2)
#
jpeg("Figure1.jpeg", width=800, height=600)
#grid.arrange(p2, p1, ncol = 2)
p1
dev.off()
#####################################################################################################

                                                    ########
                                                    #STEP 5#
                                                    ########
                                   #PLOTTING CORRELATIONS 1909 against 2000s#
#to scale both plots
spss_developments$hhi_1909_2.image<-spss_developments$hhi_1909_2*100
spss_developments$hhi_ess.image<-spss_developments$hhi_ess*100
#
fit1<-lm(nodenom_2008~norel1909 , data=spss_developments)
fit2<-lm(hhi_ess.image~hhi_1909_2.image, data=spss_developments)
summary(fit1)
summary(fit2)
#
p3<-qplot(x = norel1909, y = nodenom_2008, data = spss_developments,
      main="No church denomination in 1909 and 2000s",
      xlab= "No denomination in 1909 (census)", 
      ylab = "No denomination in 2000s (ESS)") + 
      geom_abline(intercept = fit1$coeff[1], slope = fit1$coeff[2])
#
p4<-qplot(x = hhi_1909_2.image, y = hhi_ess.image, data = spss_developments,
          main="Church concentration in 1909 and 2000s",
          xlab= "HHI in 1909 (census)", 
          ylab = "HHI in 2000s (ESS)")+ 
geom_abline(intercept = fit2$coeff[1], slope = fit2$coeff[2])
#
grid.arrange(p3, p4, ncol = 2) 
#
jpeg("Figure3.jpeg", width=800, height=600)
grid.arrange(p3, p4, ncol = 2) 
dev.off()
#
#Making comparisons by provinces (not in the paper)
spss_developments$province<-NA
#Groningen
spss_developments$province[spss_developments$minecorop=="Oost-Groningen"]<-"Groningen"
spss_developments$province[spss_developments$minecorop=="Delfzijl en omgeving"]<-"Groningen"
spss_developments$province[spss_developments$minecorop=="Overig Groningen"]<-"Groningen"
#Friesland
spss_developments$province[spss_developments$minecorop=="Noord-Friesland"]<-"Friesland"
spss_developments$province[spss_developments$minecorop=="Zuidoost-Friesland"]<-"Friesland"
spss_developments$province[spss_developments$minecorop=="Zuidwest-Friesland"]<-"Friesland"
#
spss_developments$province[spss_developments$minecorop=="Noord-Drenthe"]<-"Drente"
spss_developments$province[spss_developments$minecorop=="Zuidoost-Drenthe"]<-"Drente"
#
spss_developments$province[spss_developments$minecorop=="Noord-Overijssel"]<-"Overijssel"
spss_developments$province[spss_developments$minecorop=="Zuidwest-Overijssel"]<-"Overijssel"
spss_developments$province[spss_developments$minecorop=="Twente"]<-"Overijssel"
#
spss_developments$province[spss_developments$minecorop=="Veluwe"]<-"Gelderland"
spss_developments$province[spss_developments$minecorop=="Zuidwest-Gelderland"]<-"Gelderland"
spss_developments$province[spss_developments$minecorop=="Achterhoek"]<-"Gelderland"
spss_developments$province[spss_developments$minecorop=="Arnhem/Nijmegen"]<-"Gelderland"
#
spss_developments$province[spss_developments$minecorop=="Flevoland"]<-"Flevoland"
#
spss_developments$province[spss_developments$minecorop=="Utrecht"]<-"Utrecht"
#
spss_developments$province[spss_developments$minecorop=="Kopvan Noord-Holland"]<-"Noord-Holland"
spss_developments$province[spss_developments$minecorop=="Alkmaar e.o."]<-"Noord-Holland"
spss_developments$province[spss_developments$minecorop=="IJmond"]<-"Noord-Holland"
spss_developments$province[spss_developments$minecorop=="Agglomeratie Haarlem"]<-"Noord-Holland"
spss_developments$province[spss_developments$minecorop=="Zaanstreek"]<-"Noord-Holland"
spss_developments$province[spss_developments$minecorop=="Groot-Amsterdam"]<-"Noord-Holland"
spss_developments$province[spss_developments$minecorop=="Het Gooi en Vechtstreek"]<-"Noord-Holland"
#
spss_developments$province[spss_developments$minecorop=="Agglomeratie Leiden en Bollenstreek"]<-"Zuid-Holland"
spss_developments$province[spss_developments$minecorop=="Gravenhage"]<-"Zuid-Holland"
spss_developments$province[spss_developments$minecorop=="Delft en Westland"]<-"Zuid-Holland"
spss_developments$province[spss_developments$minecorop=="Oost-Zuid-Holland"]<-"Zuid-Holland"
spss_developments$province[spss_developments$minecorop=="Groot-Rijnmond"]<-"Zuid-Holland"
spss_developments$province[spss_developments$minecorop=="Groot-Amsterdam"]<-"Zuid-Holland"
spss_developments$province[spss_developments$minecorop=="Zuidoost-Zuid-Holland"]<-"Zuid-Holland"
#
spss_developments$province[spss_developments$minecorop=="Overig Zeeland"]<-"Zeeland"
spss_developments$province[spss_developments$minecorop=="Zeeuwsch-Vlaanderen"]<-"Zeeland"
#
spss_developments$province[spss_developments$minecorop=="West-Noord-Brabant"]<-"Noord-Brabant"
spss_developments$province[spss_developments$minecorop=="Midden-Noord-Brabant"]<-"Noord-Brabant"
spss_developments$province[spss_developments$minecorop=="Noordoost-Noord-Brabant"]<-"Noord-Brabant"
spss_developments$province[spss_developments$minecorop=="Zuidoost-Noord-Brabant"]<-"Noord-Brabant"
#
spss_developments$province[spss_developments$minecorop=="Noord-Limburg"]<-"Limburg"
spss_developments$province[spss_developments$minecorop=="Midden-Limburg"]<-"Limburg"
spss_developments$province[spss_developments$minecorop=="Zuid-Limburg"]<-"Limburg"
#
table(spss_developments$province)
#
data.prov<-data.frame(hhi1909=tapply(spss_developments$hhi_1909_2, spss_developments$province,mean))
data.prov$province<-row.names(data.prov)
#
data.prov<-merge(data.prov,hhi_cbs,by="province")
data.prov.long<-melt(data.prov, id.vars="province")
#
positions <- data.prov$province[order(data.prov$hhi1909)]
bplot<-ggplot(data.prov.long,aes(province,value))
bplot+geom_bar(aes(fill = variable), position = "dodge", stat="identity")+ scale_x_discrete(limits = positions)
#
#####################################################################################################
                                              
                                                   ########
                                                   #STEP 6#
                                                   ########

#Appendix
#Bootstrap
names(hinl)
#Making a new dataset
hinl_relgroups<-hinl[,c(34,36:50)]
hinl_relgroups_corop_split<-split(hinl_relgroups,hinl_relgroups$minecorop)
#create a dummy data.frame to fill in with new simulated variables
hinl_relgroups_corop_split_bstr<-list()
for (i in 1:39){
  hinl_relgroups_corop_split_bstr[[i]]<-hinl_relgroups_corop_split[[i]][,2:16]
}
#
#Empty data for a function
results.sim<-NA
results.real<-NA
real.norel1909<-list()
real.hhi.1909<-list()
list.simulations<-list()
#
for (i in 1:39){
hinl_relgroups_corop_split_bstr[[i]]$totalpopulation<-apply(hinl_relgroups_corop_split_bstr[[i]][,1:15],1,sum, na.rm=T)
hinl_relgroups_corop_split_bstr[[i]]$religiouspopulation<-apply(hinl_relgroups_corop_split_bstr[[i]][,1:14],1,sum, na.rm=T)
hinl_relgroups_corop_split_bstr[[i]]$religious1909<-hinl_relgroups_corop_split_bstr[[i]]$religiouspopulation/hinl_relgroups_corop_split_bstr[[i]]$totalpopulation*100
hinl_relgroups_corop_split_bstr[[i]]$hhi.1909<-apply((hinl_relgroups_corop_split_bstr[[i]][,1:14]/hinl_relgroups_corop_split_bstr[[i]]$religiouspopulation)^2,1,sum, na.rm=T)

}
#
names(hinl_relgroups_corop_split_bstr[[1]])
#
hinl_relgroups_corop_split_bstr_sim<-hinl_relgroups_corop_split_bstr
#New Function
for (k in 1:1000){ #the correlation between hhi_1909 and no_religious_1909 will be calculated 1000 times
  for(i in 1:39 ){   #correlations in each of the 39 COROPS
    for (j in 1:15){ #15 religious groups to be simulated / bootstrapped
      hinl_relgroups_corop_split_bstr_sim[[i]][,j]<-sample(hinl_relgroups_corop_split_bstr[[i]][,j], replace=T)      
    } 
    #now, after all variables are simulated, lets caclulate the number of total population,
    #the number of religious population,
    #the number of non religious people,
    #the number of hhi_simulated:
    hinl_relgroups_corop_split_bstr_sim[[i]]$totalpopulation<-apply(hinl_relgroups_corop_split_bstr_sim[[i]][,1:15],1,sum, na.rm=T)
    hinl_relgroups_corop_split_bstr_sim[[i]]$religiouspopulation<-apply(hinl_relgroups_corop_split_bstr_sim[[i]][,1:14],1,sum, na.rm=T)
    hinl_relgroups_corop_split_bstr_sim[[i]]$religious1909<-hinl_relgroups_corop_split_bstr_sim[[i]]$religiouspopulation/hinl_relgroups_corop_split_bstr_sim[[i]]$totalpopulation*100
    hinl_relgroups_corop_split_bstr_sim[[i]]$simulated.norel.1909<-100-hinl_relgroups_corop_split_bstr_sim[[i]]$religious1909
    hinl_relgroups_corop_split_bstr_sim[[i]]$simulated.hhi.1909<-apply((hinl_relgroups_corop_split_bstr_sim[[i]][,1:14]/hinl_relgroups_corop_split_bstr_sim[[i]]$religiouspopulation)^2,1,sum, na.rm=T)
    #
    #Correlations:
    results.sim[i]<-cor(hinl_relgroups_corop_split_bstr_sim[[i]]$religious1909,hinl_relgroups_corop_split_bstr_sim[[i]]$simulated.hhi.1909, use="complete.obs")
    results.real[i]<-cor(hinl_relgroups_corop_split_bstr[[i]]$religious1909,hinl_relgroups_corop_split_bstr[[i]]$hhi.1909, use="complete.obs")
  }
  list.simulations[[k]]<-results.sim #repeat 1000 times
  final.list<-do.call(cbind,list.simulations) #make one data
  final.cors<-round(apply(final.list,1,mean,na.rm=T),3) #look at the average
}
#Check the distribution of the correlations for one of the COROPS
hist(final.list[30,],xlab="Simulated Correlations", main="Zaanstreek")
abline(v=results.real[30], col="red",lwd=8,lty="dotted")
#Check the distribution of the correlations for one of the COROPS
hist(final.list[19,],xlab="Simulated Correlations", main="Noord-Limburg")
abline(v=results.real[19], col="red",lwd=8,lty="dotted")
#New data frame
df<-data.frame(corops=names(table(hinl$minecorop)),correls.sim=final.cors,
               correls.real=results.real)
#
df2<-melt(df, id="corops")
df2$COROP<-as.character(df2$corops)
df2$correls<-as.numeric(df2$variable)
str(df2)
#
pnew<-ggplot(df2, aes(corops,value)) +geom_point(aes(colour = variable, shape=variable))+ geom_line() +coord_flip()
pnew+scale_x_discrete()+labs(y = "Correlation")

#limits=corops
jpeg("Figure6.jpeg", width=800, height=600)
par(mfrow=c(1,2))
hist(final.list[30,],xlab="Simulated Correlations", main="Zaanstreek")
abline(v=results.real[30], col="red",lwd=8,lty="dotted")
#Check the distribution of the correlations for one of the COROPS
hist(final.list[19,],xlab="Simulated Correlations", main="Noord-Limburg")
abline(v=results.real[19], col="red",lwd=8,lty="dotted")
dev.off()
#
jpeg("Figure4.jpeg", width=800, height=600)
pnew+scale_x_discrete()+labs(y = "Correlation")
dev.off()



###########################################################################################################
                                    