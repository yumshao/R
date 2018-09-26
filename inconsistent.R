median<-apply(USArrests,2,median)
mads<-apply(USArrests,2,mad)
arrest<-scale(USArrests,center=median,scale=mads)
arrest.dist<-dist(arrest)
arrest.hclust<-hclust(arrest.dist,method="average")
plot(arrest.hclust,hang=-1)
Z=arrest.hclust;
inconsistency=NULL;
for(k in 1:length(Z$height))
{
  count=0;
  a=NULL;
  a=append(a,Z$height[k]);
  for (i in 1:length(Z$merge[k,]))
  {
    if(Z$merge[k,i]>0)
    {
      a=append(a,Z$height[Z$merge[k,i]]);
      count=count+1
    }
  }
if (count==0)
{inconsistency=append(inconsistency,0);}
  else
  {inconsistency=append(inconsistency,(Z$height[k]-mean(a))/sd(a));
  }
}
print(inconsistency)

