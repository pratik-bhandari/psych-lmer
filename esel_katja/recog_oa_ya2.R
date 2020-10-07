# read in both data frames and combine

oa<-read.csv("C:/Users/Katja/Dropbox/SPR_PAPER/OA/RECOGNITION_TASK/ESEL_OA_RECOGNITION_preprocessed.csv")
str(oa)
oa$group<-c("Older Adults")
ya<-read.csv("C:/Users/Katja/Dropbox/SPR_PAPER/OA/RECOGNITION_TASK/ESEL_YA_RECOGNITION_preprocessed.csv")
str(ya)
ya$group<-c("Younger Adults")
og<-rbind.fill(oa,ya)
str(og)

# set up condition3 factor
og$condition<-factor(of$condition)
og$condition3<-as.factor(ifelse(is.na(og$condition),"new", paste(og$condition)))
str(og)

og$group<-factor(og$group)

og$group<-relevel(og$group, ref="Younger Adults")
levels(og$group)


# exclude wrong combo ppl
og2<-subset(og, og$wrong_combo=="")
nrow(og2)
nrow(og)
og2$Subject<-droplevels(og2$Subject)


og2$scond<-og2$condition3

## contrast coding for condition3
levels(og2$condition3)

### plot RTs depending on condition3
cog2<-cast(og2, Subject+condition3~., mean, value="word.RT", na.rm=T)
names(cog2)[3]<-"rt"

ggplot(cog2, aes(x=condition3, y=rt))+
  geom_boxplot() +
  theme_bw(base_size=20) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="red")





# reorder factor condition3
library(DescTools)
og2$condition3 <- reorder(og2$condition3, new.order=c(2, 3, 1))
levels(og2$condition3)


# contrasts using stats idre
# https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/

my.helmert = matrix(c(2/3,-1/3, -1/3, 0, 2/3, -1/3), ncol = 2)
my.helmert


contrasts(og2$condition3) = my.helmert
og2$condition3
#contrast 1 new vs old
#contrast 2 unexpected vs expected


any(is.na(og2$word.RT)) # no NAs in RT column
tapply(og2$word.RT, og2$condition3, mean)
tapply(og2$word.RT, og2$condition3, mean, na.rm=T)
#new        unexpected      expected 
#2083.268   2280.566       1927.308 
#contrast 1 new vs old should be 2083.268  -((2280.566+1927.308)/2) = -20.669
#contrast 2 unexpected vs expected should be, 2280.566 -1927.308 = 353.258

tr<-lm(word.RT~condition3, data=og2)
summary(tr)

# a) first contrast in model output is 38.21 and not -20.669 (what I expected) ....
# b) what's the intercept? it's not the grand mean ...



## helmert different coding
## https://marissabarlaz.github.io/portfolio/contrastcoding/


levels(og2$scond)

og2$scond<-factor(og2$scond, levels=c("new", "unexpected", "expected"))
levels(og2$scond)

myhelmert2 = matrix(c(2/3, -1/3, -1/3, 0, .5, -.5), ncol = 2)
myhelmert2

contrasts(og2$scond)<-myhelmert2

og2$scond

tapply(og2$word.RT, og2$condition3, mean)
#expected        new     unexpected 
#1927.308   2083.268      2280.566 

# contr1 should be 2083.268-((1927.308+ 2280.566 )/2)=-20.669
# contr2 should be 2280.566-1927.308=353.258
# grand mean =2097.047

tr<-lm(word.RT~scond, data=og2)
summary(tr)

#####################
#####################
#####################
#####################
#####################
#####################


# IGNORE WHATS BELOW


#####################
#####################
#####################
#####################
#####################


cog<-cast(og2, Subject+group +condition3~., mean, value="word.ACC", na.rm=T)
names(cog)[4]<-"acc"


library(ggplot2)
ggplot(cog, aes(x=condition3, y=acc))+
  geom_boxplot()+
  theme_bw(base_size=20)+
  facet_wrap(~group)+
  coord_cartesian(ylim=c(0,1)) +
  ylab("accuracy")+
  xlab("") +
  stat_summary(fun.y=mean, shape=17, geom="point", size=3)+
  theme(axis.text.x = element_text(angle = 20))


levels(og2$condition3)
library(DescTools)
og2$condition3 <- reorder(og2$condition3, new.order=c(2, 3, 1))
levels(og2$condition3)

og2$mycon<-contr.helmert(og2$condition3)
og2$mycon

levels(og2$condition3)
og2$condition3 <- reorder(og2$condition3, new.order=c(2, 3, 1))
levels(og2$condition3)



mod<-glmer(word.ACC~condition3*group +
        (1|Subject)+
        (1|noun), data=og2, family=binomial)
summary(mod)
