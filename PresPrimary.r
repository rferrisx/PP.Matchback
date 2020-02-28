# February 2020 Presidential Primary Election:
setwd("D:\\Politics\\PP.2020")
library(data.table)
library(lubridate)
fsum <- function(x) {sum(x,na.rm=TRUE)}
# D:\Politics\PP.2020\ballotstatus_02252020
# D:\Politics\PP.2020\ballotstatus_02262020
# D:\Politics\PP.2020\ballotstatusreport_02272020

setwd("D:\\Politics\\PP.2020")
# load("poLCA_m1.GE.2019") optional GE.2019 predictors

PP.2020 <- fread("D:\\Politics\\PP.2020\\ballotstatusreport_02272020\\BallotStatusReport_02272020.csv")
colnames(PP.2020) <- gsub(" ","",names(PP.2020))
PP.2020[,Age:=round(as.integer(ymd(as.Date(now())) - as.Date(mdy(gsub(" 0:00","",DOB))))/365)]

# duplicated VoterID
PP.2020[duplicated(VoterID),.N]

# Generic BallotStatus
PP.2020[,.N,.(BallotStatus)][order(-N)]
PP.2020[,.N,.(Party)][order(-N)]
PP.2020[,.N,.(BallotStatus,Party)][,dcast(.SD,Party ~ BallotStatus,value.var = "N",fun.aggregate=fsum)]

# Generic Party Choices by County and/or BallotStatus 
PP.2020[,.N,.(County,Party)][,dcast(.SD,County ~ Party,value.var="N",fun.aggregate=fsum)]
PP.2020[Party == "NEI",.N,.(County,BallotStatus)][,dcast(.SD,County ~ BallotStatus,value.var="N",fun.aggregate=fsum)]
PP.2020[,.N,.(BallotStatus,Party)][,dcast(.SD,Party ~ BallotStatus,value.var = "N",fun.aggregate=fsum)][order(-Rejected)]

# No Party Rejected merge routines
l_ <- PP.2020[,.N,.(County)]
l1 <- PP.2020[,.N,.(County,Party,BallotStatus)][,dcast(.SD,County ~ BallotStatus,value.var="N",fun.aggregate=fsum)]
l2 <- PP.2020[Party=="NEI",.N,.(County,Party,BallotStatus)][,dcast(.SD,County ~ BallotStatus,value.var="N",fun.aggregate=fsum)]
l3 <- merge(l1,l2[,.(County,NoPartyReceived=Received,NoPartyRejected=Rejected)],by="County",all.x=TRUE)
l4 <- l3[,.SD[,
.(PCT.Received.No.Party=round(NoPartyReceived/Received,3) * 100,
PCT.Reject.No.Party=round(NoPartyRejected/Rejected,3) * 100)],
.(County,Accepted,Received,Rejected,NoPartyReceived,NoPartyRejected)]
l5 <- merge(l_[,.(County,Ballots=N)],l4,by="County",all.x=TRUE)
l5[]
l5[Ballots > 9000,][order(-Ballots)]
write.csv( l5[Ballots > 9000,][order(-Ballots)], "BallotsRejectedNoParty.csv",row.names=FALSE)

# Additional Rejected Analysis
PP.2020[BallotStatus == "Accepted",.N,.(County)][order(-N)]

cnames <- PP.2020[BallotStatus == "Rejected",.N,
.(ChallengeReason,County)][,
dcast(.SD,County ~ ChallengeReason,value.var="N",fun.aggregate=fsum)][,
gsub(" ","",names(.SD))]
cnames <- gsub(",","",cnames)
jkl <- merge( PP.2020[BallotStatus == "Accepted",.(Accepted.Ballots =.N),.(County)][order(-Accepted.Ballots)],
(PP.2020[BallotStatus == "Rejected",
.N,.(ChallengeReason,County)][,
setnames(dcast(.SD,County ~ ChallengeReason,value.var="N",fun.aggregate=fsum),cnames)]),
by="County",all.x=TRUE )[order(-Accepted.Ballots)]
jkl[]
write.csv(jkl,"jkl.csv",row.names=FALSE)

# Not all "No Party" (e.g. 'NEI' or neither) are categorized as "No Party" challenges.
PP.2020[Party == "NEI" & (ChallengeReason != "No Party" & ChallengeReason != "No Party, No Sig"),
.N,.(County,Party,BallotStatus,ChallengeReason)][order(-N)]

PP.2020[Party =="",.N,.(County,BallotStatus)]

# Top Challenged Precinct (Computationally expensive)
ChallengesXPrecinct <- merge(
PP.2020[BallotStatus == "Rejected",.N,.(ChallengeReason,County,Precinct)][,
	dcast(.SD,Precinct+County ~ ChallengeReason,value.var="N",fun.aggregate=fsum)],

PP.2020[BallotStatus == "Rejected",.N,.(ChallengeReason,County,Precinct)][,
	dcast(.SD,Precinct+County ~ ChallengeReason,value.var="N",fun.aggregate=fsum)][,.SD[,.(rowSums=rowSums(.SD[,3:25]))],.(Precinct)]
)

ChallengesXPrecinct[,.(Precinct,County,TopChallengedPrecinct.rowSums=rowSums,NoParty=.SD[,c(15,16)])][order(-TopChallengedPrecinct.rowSums)][1:100]
ChallengesXPrecinct[County == "Whatcom",.(Precinct,County,TopChallengedPrecinct.rowSums=rowSums,NoParty=.SD[,c(15,16)])][order(-TopChallengedPrecinct.rowSums)][1:100]

# density charts for All County Challenges
dev.new()
par(mfrow=c(1,7))
PP.2020[BallotStatus == "Accepted",.(Age)][order(Age)][,plot(density(Age),main="All Accepted")]
PP.2020[BallotStatus == "Rejected",.(Age)][order(Age)][,plot(density(Age),main="All Rejected")]
PP.2020[BallotStatus == "Rejected" & ChallengeReason == "No Party",.(Age)][order(Age)][,plot(density(Age),main="No Party")]
PP.2020[BallotStatus == "Rejected" & ChallengeReason == "No Party, No Sig",.(Age)][order(Age)][,plot(density(Age),main="No Party No Sig")]
PP.2020[BallotStatus == "Rejected" & ChallengeReason == "Review",.(Age)][order(Age)][,plot(density(Age),main="Review")]
PP.2020[BallotStatus == "Rejected" & ChallengeReason == "Signature Does Not Match",.(Age)][order(Age)][,plot(density(Age),main="Signature No Match")]
PP.2020[BallotStatus == "Rejected" & ChallengeReason == "Unsigned",.(Age)][order(Age)][,plot(density(Age),main="Unsigned")]
par(mfrow=c(1,1))


# Quick Demographics:
# Age Chart
dev.new()
PP.2020[,.N,.(Age)][order(Age)][,barplot(N,names.arg=Age,col=rainbow(nrow(.SD)))]

# Gender Table
PP.2020[,.N,.(Gender)]

# County Tables
PP.2020[,.N,.(County)]
PP.2020[,.N,.(County,BallotStatus)][,dcast(.SD,County ~ BallotStatus,value.var="N",fun.aggregate=fsum)]

# Received and Sent Date:
PP.2020[,.N,.(SentDate=mdy_hm(SentDate))][order(-N)][1:20]
PP.2020[,.N,.(ReceivedDate=mdy_hm(ReceivedDate))][order(-N)][1:20]


## Whatcom County Specific Analysis
# Number of Rejected
PP.2020[County == "Whatcom" & BallotStatus == "Rejected",.N]
PP.2020[County == "Whatcom" & BallotStatus == "Rejected",.N,.(ChallengeReason)]

# density charts for Whatcom Challenges
dev.new()
par(mfrow=c(1,7))
PP.2020[County == "Whatcom" & BallotStatus == "Accepted",.(Age)][order(Age)][,plot(density(Age),main="All Accepted")]
PP.2020[County == "Whatcom" & BallotStatus == "Rejected",.(Age)][order(Age)][,plot(density(Age),main="All Rejected")]
PP.2020[County == "Whatcom" & BallotStatus == "Rejected" & ChallengeReason == "No Party",.(Age)][order(Age)][,plot(density(Age),main="No Party")]
PP.2020[County == "Whatcom" & BallotStatus == "Rejected" & ChallengeReason == "No Party, No Sig",.(Age)][order(Age)][,plot(density(Age),main="No Party No Sig")]
PP.2020[County == "Whatcom" & BallotStatus == "Rejected" & ChallengeReason == "Review",.(Age)][order(Age)][,plot(density(Age),main="Review")]
PP.2020[County == "Whatcom" & BallotStatus == "Rejected" & ChallengeReason == "Signature Does Not Match",.(Age)][order(Age)][,plot(density(Age),main="Signature No Match")]
PP.2020[County == "Whatcom" & BallotStatus == "Rejected" & ChallengeReason == "Unsigned",.(Age)][order(Age)][,plot(density(Age),main="Unsigned")]
par(mfrow=c(1,1))

# Whatcom Precincts by Challenges
ChallengesXPrecinct <- merge(
PP.2020[County == "Whatcom" & BallotStatus == "Rejected",.N,.(ChallengeReason,Precinct)][,
	dcast(.SD,Precinct ~ ChallengeReason,value.var="N",fun.aggregate=fsum)],

PP.2020[County == "Whatcom" & BallotStatus == "Rejected",.N,.(ChallengeReason,Precinct)][,
	dcast(.SD,Precinct ~ ChallengeReason,value.var="N",fun.aggregate=fsum)][,.SD[,.(rowSums=rowSums(.SD[,2:10]))],.(Precinct)]
)

ChallengesXPrecinct[order(-rowSums)][1:20]

