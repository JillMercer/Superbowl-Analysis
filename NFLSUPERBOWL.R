#Predicts teams going to Superbowl
TeamOdds=read.csv("OddsToSuperBowl.csv", header=TRUE )
NFCWinner=NULL
AFCWinner=NULL
NFCFINAL=NULL
AFCFINAL=NULL
for(j in 1:1000) {
  NFCWinner=sample(TeamOdds$NFC,size=1,replace = FALSE,prob = TeamOdds$NFCODDS)
  AFCWinner=sample(TeamOdds$AFC,size=1,replace = FALSE,prob = TeamOdds$AFCODDS)
  NFCFINAL=rbind(NFCFINAL,data.frame(NFCWinner))
  AFCFINAL=rbind(AFCFINAL,data.frame(AFCWinner))
}
summary(NFCFINAL)
summary(AFCFINAL)


#Parses out the winning team's string
winner1=summary(NFCFINAL)
winner2=summary(AFCFINAL)

team1=trimws(gsub( ":.*","",winner1[1]))
team2=trimws(gsub(":.*","",winner2[1]))

Revenue=read.csv("Stats.csv")
Percent=NULL


#Loop to run the random percent earning calculations (modified bootsrap)
for(j in 1:1000)
{
  #Random weights to multiply to the weight of teams
  eastweight=runif(1,min=0.5,max=1)
  midweight=runif(1,min=0.1,max=0.5)
  westweight=-(runif(1,min=0.01,max=0.1))
  
    for(i in 1:length(Revenue$Team))
    {
      if(Revenue$Team[i]==team1)
      {
        Revenue$region[i]
        if(Revenue$region[i]=="east")
          team1weight=Revenue$regionweight[i]*eastweight
        if(Revenue$region[i]=="mid")
          team1weight=Revenue$regionweight[i]*midweight
        else
          team1weight=Revenue$regionweight[i]*westweight
      }
      if(Revenue$Team[i]==team2)
      {
        if(Revenue$region[i]=="east")
          team2weight=Revenue$regionweight[i]*eastweight
        if(Revenue$region[i]=="mid")
          team2weight=Revenue$regionweight[i]*midweight
        else
          team2weight=Revenue$regionweight[i]*westweight
      }
    }
  PercentReturned=(team1weight+team2weight)*100
  Percent=rbind(Percent,data.frame(PercentReturned))

}


#Exports to excel file and create arima model
data=data.frame(team1,team2,Percent)
write.csv(data,"Results.csv",row.names=TRUE)
fit=ts(Percent)
model=auto.arima(fit)
summary(model)
plot(model$x,col="red")
lines(fitted(model),col="blue")


#Creates timeplots of regions
Timeplot=read.csv("Timeplot.csv")

EastTS= ts(data=Timeplot[,2:16], start=c(2001),frequency = 1)
MidTS= ts(data=Timeplot[,17:29], start=c(2001),frequency = 1)
WestTS= ts(data=Timeplot[,30:33], start=c(2001),frequency = 1)
autoplot(EastTS)+ggtitle("East Teams Revenue 2001-2018")+ylab("Revenue in Millions")+xlab("Year")
autoplot(MidTS)+ggtitle("Midwest Teams Revenue 2001-2018")+ylab("Revenue in Millions")+xlab("Year")
autoplot(WestTS)+ggtitle("West Teams Revenue 2001-2018")+ylab("Revenue in Millions")+xlab("Year")
