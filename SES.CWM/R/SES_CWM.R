
SES_CWM=function(abuncom1,abuntot,trait,n,replicate)
{
traitvaule=sum(trait*abuncom1)/sum(abuncom1)
abunT=matrix(abuntot,length(abuntot),2)
abunT[,2]=1:length(abuntot)
nullvaule=1:replicate
for (j in 1:replicate)
{
  a=sample(abunT[,2],n,T,prob=abunT[,1])
  abunnull=abuntot
  for(i in 1:length(abuntot))
  {
    abunnull[i]=length(which(a==i))
  }
  
  nullvaule[j]=sum(trait*abunnull)/sum(abuncom1)  
}
CWM=traitvaule
SES_CWM=(traitvaule-mean(nullvaule))/sd(nullvaule)
mean_null=mean(nullvaule)
p_vaule=min(pnorm(SES_CWM),1-pnorm(SES_CWM))*2
output=list(CWM=CWM,SES_CWM=SES_CWM,mean_null=mean_null,p_vaule=p_vaule)
output
}

