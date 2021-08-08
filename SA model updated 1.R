n<-1000
##number of agents
m1 <- 100
##initial wealth
t <- 100
##simulation cycles
t<-t*12

d<-15
##depreciation rate

md<-d*12
##monthly depreciation rate

CperWR<-4
##ratio of capital to worker limit

##wage theft
wt<-1


agent <- matrix(nrow = 9, ncol = n)
rownames(agent)<-c("Money", "Employ_ind","id", "wage_level", "ConCapital", "VarCapital", "Revenue","N_employees", "wage_exp")
##agent has attributes money, employment index for workers, employment index for employers,
##wage level, and constant capital for agents that are employers
agent["Employ_ind",]<-0
##everyone initially set to unemployed
agent["id",]<-1:n
##id set
agent["Money",]<-m1
##initial wealth set
agent["ConCapital",]<-0
agent["wage_level",]<-0
agent["VarCapital",]<-0
agent["Revenue",]<-0
agent["N_employees",]<-0

##create annualized data matrix
anVarCap<-matrix(0, nrow = n, ncol = (t/12) )
anConCap<-matrix(0, nrow = n, ncol = (t/12) )
anRev<-matrix(0, nrow = n, ncol = (t/12) )


anEmployeesTemp<-matrix(nrow = 12, ncol = n)
anEmployeesFin<-matrix(nrow = n, ncol = (t/12))
  

conexT<-0
##consumption expenditure for T

wageshare<-matrix(0, nrow = 1, ncol = t)
caprev<-matrix(0, nrow = 1, ncol = t)

mw <- 25
##average wage
wagedis <- rnorm(100, mean = mw, sd = 1)
wagedis <- round(wagedis, digits = 0)
wagedis<-wagedis[(wagedis>0)]

agent["wage_exp",]<-mw
##sets original wage expectations for all agents

EmployM<-matrix( nrow = n, ncol = t)
##Matrix for employment variable

FirmbM<-matrix(0, nrow = 1, ncol = t)
##matrix for firm birth
FirmdM<-matrix(0, nrow = 1, ncol = t)
#matrix for firm death

WageM<-matrix( nrow = 1, ncol = t)
WageM[1]<-mw
#Matrix for wage variable

MoneyM<-matrix( nrow = n, ncol = t)
##Matrix for money variable

RevM<-matrix( nrow = n, ncol = t)
##matrix for firm revenue

ConcapM<-matrix( nrow = n, ncol = t)
#matrix for constant capital

VarcapM<-matrix(0, nrow = n, ncol = t)
#Matrix for for variable capital 

WageLevM<-matrix(nrow = n, ncol = t)
#matrix for wage levels

NemployeesM<-matrix(nrow = n, ncol = t)
#matrix for wage levels

CexpendM<-matrix(0, nrow=1, ncol = t)
##matrix for consumption expenditure pool of money

FixexpendM<-matrix(0, nrow = 1, ncol = t)
##matrix for fixed capital expenditure pool of money 

##select first time iteration 
ti<-1
ta<-1
u1<-1
##first step, select active agent
start_time<-Sys.time()
while(ti<=t)
{ 

  agent["ConCapital",]<-round(agent["ConCapital",]-(agent["ConCapital",]/md));
  ##depreciation

  ##creates list of agent ids for their turns to act
  a<-matrix(nrow = 1, ncol = n)
  a[1,]<-c(sample(1:n, n, replace = FALSE))

  
  am<-1
  ##loop to go through each agent's actions for a month
  while (am<length(a))
  {
    if (agent["Employ_ind",(a[,am])]>=0 )
      ##Hiring process 
    {
      eagents<-(agent["Employ_ind",]<= 0) & (agent["id",] != a[,am])&((agent["Money",])>0)&(agent["id",] !=agent["Employ_ind",a[,am]])

      H<-matrix(nrow = 2, 
                ncol = length(agent["id", eagents]));
      H[1,]<-agent["id", eagents];
      H[2,]<-(agent["Money", eagents])
      ##creates list of not employed agents, potential employers
      H[1,]<-H[1,order(H[2,])];
      H[2,]<-sort(H[2,]);
      Hprob<-(H[2,]/sum(H[2,]));
      #creates weighted probabilities by wealth
      E1<-sample( H[1,], 1, prob = Hprob);
      ##selects random employer
      
      wsample<-sample(c(agent["wage_exp",a[,am]], round(agent["wage_exp",a[,am]]*1.05)), 1)
      ##selects wage offer, either wage expectation or 10% more than wage expectation
      
      if ((((agent["Money", E1]))>(wsample*(1+ (CperWR/10) ))) &(wsample>=agent["wage_level",a[,am]])) {
        ##if offer is greater than or equal to the agent's current wage level, accept job offer AND offer plus
        ##the capital intensity limit is less than the potential employer's cash reservers employee gets hired
        
        EID<-agent["Employ_ind",(a[,am])];
        
        agent["Employ_ind",(a[,am])]<-E1;
        #adds employer's id to employees Employer index
        agent["wage_level", (a[,am])]<-wsample
        ##set employee wage level
        agent["wage_exp",a[,am]]<-wsample;
        ##wage expectation is updated to be equal to new wage level
        
        if (agent["Employ_ind", E1]==(0))
        { ##if agent's new employer was formerly unemployed, categorize them as an employer with one employee
          FirmbM[,ti]<- FirmbM[,ti]+1; 
          agent["Employ_ind", E1]<-(-1);
          agent["N_employees", E1]<-1;
        }
        
        else
        {
          ##if agent's new employer was already a capitalist, add an employee to their firm
          agent["N_employees", E1]<- agent["N_employees", E1]+1;
         
        }
        
 
        if(EID!=0)
        {
          ##if agent was employed and is switching jobs, decrease their old employer's employees by one
          agent["N_employees",EID]<-agent["N_employees",EID]-1;
          
          
        }
        

        
        
      }
      else
      { 
        ##if agent isn't hired, decrease wage expecations by 5%

            agent["wage_exp",a[,am]]<-round(agent["wage_exp",a[,am]]*.9);
 
      }
    }
    
    
    ##have random other agent consume on market
    conagent<-sample(agent["id",(agent["Money",]>0 & agent["id",]!=(a[,am]))], 1)
    EX1<- sample(1:agent["Money",conagent], 1);
    conexT<-conexT+EX1;
    CexpendM[,ti]<-CexpendM[,ti]+EX1;
    agent["Money",conagent]<-agent["Money",conagent]-EX1;
    ##agent expends money for consumption, putting it in total consumption pool
    
    
    
    ##for worker agents
    if (agent["Employ_ind",(a[,am])]>0)
    {     
      
      ##sample market
      cc1<-round((agent["ConCapital",agent["Employ_ind",(a[,am])]]/(md))/((agent["N_employees",agent["Employ_ind",(a[,am])]])+1));
      ##get fixed capital per laborers 
      s<-conexT-cc1;
      
      if (s>0)
      {
        s1<-sample(1:s, 1)+cc1;
        #set sale price
        conexT<-conexT-s1;
        agent["Money", agent["Employ_ind",(a[,am])]]<-agent["Money", agent["Employ_ind",(a[,am])]]+s1;
        agent["Revenue", agent["Employ_ind",(a[,am])]]<- agent["Revenue", agent["Employ_ind",(a[,am])]]+s1;
        anRev[agent["Employ_ind",(a[,am])], ta]<-anRev[agent["Employ_ind",(a[,am])], ta]+s1;
        caprev[,ti]<-caprev[,ti]+s1;
        ##execute sales transaction deposit to employer's account
        
      }
      else
      {
        if (conexT>1)
        {
          s1<-sample(1:conexT, 1);
          #set sale price
          conexT<-conexT-s1;
          agent["Money", agent["Employ_ind",(a[,am])]]<-agent["Money", agent["Employ_ind",(a[,am])]]+s1;
          agent["Revenue", agent["Employ_ind",(a[,am])]]<- agent["Revenue", agent["Employ_ind",(a[,am])]]+s1;
          anRev[agent["Employ_ind",(a[,am])], ta]<-anRev[agent["Employ_ind",(a[,am])], ta]+s1;
          ##execute sales transaction deposit to employer
          caprev[,ti]<-caprev[,ti]+s1;
        }
      }
      
    } 
    
    
    
    ##if agent is an employer, add value for their labor, execute wage payment and fixed capital purchases
    if (agent["Employ_ind",(a[,am])]==-1)
    {
      ##sample market
      cc1<-round((agent["ConCapital",(a[,am])]/(md))/((agent["N_employees",(a[,am])])+1));
      ##get fixed capital per laborers 
      s<-conexT-cc1;
      
      if (s>0)
      {
        s1<-sample(1:s, 1)+cc1;
        #set sale price
        conexT<-conexT-s1;
        agent["Money", (a[,am])]<-agent["Money", (a[,am])]+s1;
        agent["Revenue", (a[,am])]<- agent["Revenue", (a[,am])]+s1;
        anRev[(a[,am]), ta]<-anRev[(a[,am]), ta]+s1;
        caprev[,ti]<-caprev[,ti]+s1;
        ##execute sales transaction deposit to employer for consumption good
        
      }
      else
      {
        if (conexT>1)
        {
          s1<-sample(1:conexT, 1);
          #set sale price
          conexT<-conexT-s1;
          agent["Money", (a[,am])]<-agent["Money", (a[,am])]+s1;
          agent["Revenue", (a[,am])]<- agent["Revenue", (a[,am])]+s1;
          anRev[(a[,am]), ta]<-anRev[(a[,am]), ta]+s1;
          ##execute sales transaction deposit to employer for consumption good
          caprev[,ti]<-caprev[,ti]+s1;
        }
      }
      
      
      ##send out wage payment
      if (agent["N_employees",a[,am]]>0)
      {
        ##create list of employees
        vcRatio<-sample(1:CperWR, 1)/10
        Wa<-matrix(nrow = 3, ncol = length(agent["id",agent["Employ_ind",]==(a[,am])]));
        Wa[1,]<-agent["id",agent["Employ_ind",]==(a[,am])];
        Wa[2,]<-agent["wage_level",agent["Employ_ind",]==(a[,am])];
        Wa[3,]<-Wa[2,]*(1+vcRatio)
        ##randomize list, withold wages for certain percent of workforce
        Wran<-sample(1:round(length(Wa[1,])), round(length(Wa[1,])*wt), replace = FALSE);
        W<-matrix(nrow = 3, ncol = length(Wran))
        W[1,]<-Wa[1, Wran];
        W[2,]<-Wa[2, Wran];
        W[3,]<-Wa[3, Wran];
        #pay employees
        paid<-(cumsum(W[3,])<agent["Money",(a[,am])]);
        agent["Money",(a[,am])] <- agent["Money",(a[,am])]-sum(W[3,paid]);
        agent["Money", W[1,paid]]<-agent["Money", W[1,paid]]+W[2,paid];
        agent["Revenue",W[1,paid]]<-agent["Revenue",W[1,paid]]+W[2,paid];
        anRev[W[1,paid], ta]<-anRev[W[1,paid], ta]+W[2,paid];
        agent["VarCapital",a[,am]]<-agent["VarCapital",a[,am]]+sum(W[2,paid]);
        anVarCap[a[,am], ta]<-anVarCap[a[,am], ta]+sum(W[2,paid]);
        wageshare[,ti]<-wageshare[,ti]+sum(W[2,paid]);
        agent["ConCapital",a[,am]]<-agent["ConCapital",a[,am]]+sum(round(W[2,paid]*vcRatio));
        FixexpendM[,ti]<-FixexpendM[,ti]+sum(round(W[2,paid]*vcRatio));
        anConCap[a[,am], ta]<-anConCap[a[,am], ta]+sum(round(W[2,paid]*vcRatio));
        conexT<-conexT+sum(round(W[2,paid]*vcRatio));
        
        
        
        ##those workers who can't be paid are laid off
        paid<-paid==FALSE;
        agent["Employ_ind",W[1,paid]]<-0;
        agent["wage_level",W[1,paid]]<-0;
      
        agent["N_employees",a[,am]]<-agent["N_employees",a[,am]]-sum(paid);
      }
      
      ##check for bankruptcies 
      if (agent["N_employees",a[,am]]==0)
      {
        agent["Employ_ind", a[,am]]<-0;
        ##select random other employer to receive capital stock
        #cProb<-agent["ConCapital", (agent["N_employees", ]>0) & (agent["id", ]!=a[,am])]/agent["N_employees", (agent["N_employees", ]>0) & (agent["id", ]!=a[,am])];
        #cProb<-cProb<=mean(cProb);
        
        newCapOwner<-(agent["id", (agent["N_employees", ]>0) & (agent["id", ]!=a[,am])]);
        #newCapOwner<-newCapOwner[cProb]
        newCapOwner<-sample(newCapOwner, 1)
        agent["ConCapital", newCapOwner]<-agent["ConCapital", newCapOwner]+agent["ConCapital",a[,am]];
        anConCap[newCapOwner, ta]<-anConCap[newCapOwner, ta]+agent["ConCapital",a[,am]];
        agent["ConCapital",a[,am]]<-0;
        agent["VarCapital",a[,am]]<-0;
        agent["Revenue",a[,am]]<-0;
        FirmdM[,ti]<-FirmdM[,ti]+1;
        ##employer becomes unemployed, capital accounts are zeroed out
      }
      
      
    }  
    
    
    
    ##moves to next agent
    am<-am+1;
    
  }
  ##update historical variables
  NemployeesM[,ti]<-agent["N_employees",];
  EmployM[,ti]<-agent["Employ_ind",];
  MoneyM[,ti]<-agent["Money",];
  ConcapM[,ti]<-agent["ConCapital",];
  WageLevM[,ti]<-agent["wage_level",];
  VarcapM[,ti]<-agent["VarCapital",];
  RevM[,ti]<-agent["Revenue",];
  agent["VarCapital",]<-0;
  agent["Revenue",]<-0;
  WageM[ti]<- mean(agent["wage_level",(agent["wage_level",]>0)]);
  mw<-mean(agent["wage_level",(agent["wage_level",]>0)]);
  anEmployeesTemp[u1,]<-agent["N_employees",];
  u1<-u1+1
  if ((ti/12)==round(ti/12))
    {
    u1<-1;
    anEmployeesFin[,ta]<-colMeans(anEmployeesTemp);
    ta<-ta+1
    }
  
  ##moves to next production cycle
  ti<-ti+1;
}
end_time<-Sys.time()
end_time-start_time


##START GRAPHICAL ANALYSIS
library(ggplot2)
library(ggQC)
library(VGAM)
##create histograms
##hist for unemployed population
unemployed<-data.frame(colSums(EmployM[1:n,100:t]==0))
colnames(unemployed)<-c("unemployed_pop")
sd1<-sd(colSums(EmployM[1:n,100:t]==0))
m1<-mean(colSums(EmployM[1:n,100:t]==0))
ggplot(data= unemployed, aes(x=unemployed_pop)) +geom_histogram(aes(y=stat(density)))+stat_function(fun = dnorm, 
                                                                                                    args = list(mean = m1, sd = sd1), col = "orange")
##hist for employed population
employed<-data.frame(colSums(EmployM[1:n,100:t]>0))
colnames(employed)<-c("employed_pop")
sd1<-sd(colSums(EmployM[1:n,100:t]>0))
m1<-mean(colSums(EmployM[1:n,100:t]>0))
ggplot(data= employed, aes(x=employed_pop)) +geom_histogram(aes(y=stat(density)))+stat_function(fun = dnorm, 
                                                                                                args = list(mean = m1, sd = sd1), col = "orange")
##hist for capitalist population
capitalist<-data.frame(colSums(EmployM[1:n,100:t]<0))
colnames(capitalist)<-c("capitalist_pop")
sd1<-sd(colSums(EmployM[1:n,100:t]<0))
m1<-mean(colSums(EmployM[1:n,100:t]<0))
ggplot(data= capitalist, aes(x=capitalist_pop)) +geom_histogram(aes(y=stat(density)))+stat_function(fun = dnorm, 
                                                                                                    args = list(mean = m1, sd = sd1), col = "orange")
mean(colSums(EmployM[1:n,100:t]<0))

##create firm demise histogram
FirmdDF<-data.frame(FirmdM[100:t])
colnames(FirmdDF)<-"Firm_Deaths"
m2<-mean(log(FirmbM+1))
sd2<-sd(log(FirmbM+1))
ggplot(data = FirmdDF, aes(x=Firm_Deaths))+geom_histogram(aes(y=stat(density)))+stat_function(
  fun = dlnorm,  args = list(mean = m2, sd = sd2), col = "orange")
(mean(FirmdM))/(mean(colSums(EmployM[1:n,]<0)))


##create firm birth histogram
FirmbDF<- data.frame(FirmbM[100:t])
colnames(FirmbDF)<-"Firm_Births"
ggplot(data = FirmbDF, aes(x=Firm_Births))+geom_histogram()

##create rate of profit distribution
VariableCapital<-VarcapM[VarcapM>0]
Vcapl<-NemployeesM[VarcapM>0]
ConstantCapital<-ConcapM[VarcapM>0]
Revenue<-RevM[VarcapM>0]
r<-(Revenue-(VariableCapital+round(ConstantCapital/md)))/(VariableCapital+round(ConstantCapital/md))
rop<-data.frame(r)
colnames(rop)<-"RoP"
minr<-min(rop)
maxr<-max(r)
ggplot(data = rop, aes(x=RoP))+geom_histogram(aes(y=stat(density)))+stat_function(
  fun = dgamma,  args = list(.44, shape=.7), col = "orange")+xlim(0,17)

#+ scale_y_log10()+ scale_x_log10()


mean(r)



##create rate of profit historical data
hisvar<-wageshare
hiscon<-colSums(ConcapM[1:n,])
hisrev<-caprev
hisR<-(hisrev-(hisvar+round(hiscon/md)))/(hisvar+round(hiscon/md))
plot(100:t,hisR[100:t], type = "l", ylab = "Rate of Profit", xlab = "Time")
mean(hisR[100:t])

#create historical constant capital graph
plot(100:t, hiscon[100:t], type = "l", ylab = "Constant Capital", xlab = "Time")

#create historical variable capital graph
plot(100:t, hisvar[100:t], type = "l", ylab = "Variable Capital", xlab = "Time")

##historical depreciation to revenue graph
plot(100:t, ((hiscon[100:t]/md)/(hisrev[100:t])), type = "l", ylab = "Constant Capital/Revenue", xlab = "Time")

##create GDP historical graph
plot(100:t,hisrev[100:t], type = "l", ylab = "Firm Revenue", xlab = "Time")

##create GDP growth graph
t3<-1
GDPgrowth<-list()
for (rv in hisrev[12:t])
{ g1<-((rv-hisrev[t])/hisrev[t]);
GDPgrowth<-c(GDPgrowth, g1);
t3<-t3+1;
}

dfgdp<-data.frame(as.numeric(GDPgrowth[100:length(GDPgrowth)]))
colnames(dfgdp)<-"growth"
gmean<-mean(as.numeric(GDPgrowth[100:length(GDPgrowth)]))
gsd<-sd(as.numeric(GDPgrowth[100:length(GDPgrowth)]))
ggplot(data = dfgdp, aes(x=growth))+geom_histogram(aes(y=stat(density)))+stat_function(fun = dnorm, 
                                                                                       args = list(mean = gmean, sd = gsd), col = "orange")

gmean
gsd


##create firm size distribution

freqdf<-data.frame(NemployeesM[NemployeesM>0])
colnames(freqdf)<-"Firm_Size"
maxr<-max(freqdf)
ggplot(data = freqdf, aes(x=Firm_Size))+geom_histogram(breaks=seq(0, 800, by=1), colour="black", fill=NA)+ scale_y_log10()+ scale_x_log10()


##create historical unemployment graph
unemploy<-(colSums(EmployM==0)/n)
plot(100:t, unemploy[100:t], type = "l", ylab = "Unemployment Rate", xlab = "time")
mean(unemploy)



##create historical wage graph
plot(100:t, WageM[100:t], type = "l", ylab = "Mean Wage", xlab = "time")
mean(WageM[WageM>0])

##create unemployment rate distribution
hist(unemploy[100:t])

##create income cumulative distribution graph
intable<-(table(round(RevM)))
indf<-data.frame(intable[2:length(intable)])
intable<-(intable[2:length(intable)])
intable<-1-(cumsum(intable)/sum(intable))
indf["Freq"]<-data.frame(intable)
var1<-sort(unique(round(as.numeric(RevM))))
indf["Var1"]<-var1[2:length(var1)]

ggplot(data = indf, aes(x=Var1, y=Freq))+geom_point()+ scale_y_log10()+ scale_x_log10()+xlab("Income")
 

##culmulative worker vs capitalist income
intable<-(table(round(RevM[EmployM<0])))
indf<-data.frame(intable[2:length(intable)])
intable<-(intable[2:length(intable)])
intable<-1-(cumsum(intable)/sum(intable))
indf["Freq"]<-data.frame(intable)
var1<-sort(unique(round(as.numeric(RevM[EmployM<0]))))
indf["Var1"]<-var1[2:length(var1)]
pareto3<-(rpareto(1000000, scale = 3e3, shape = 2.8)*.25)
pareto1<-(table(round(pareto3)))
pareto1<-1-cumsum(pareto1)/sum(pareto1)
pareto2<-data.frame(pareto1)
colnames(pareto2)<-"Freq"
pareto2["Income"]<-sort(unique(round(pareto3)))
ggplot(data = indf, aes(x=Var1, y=Freq))+geom_point()+xlab("Capitalist Income")+geom_line(data = pareto2, 
    aes(x=Income, y=Freq), col ="orange")+scale_x_log10(limit = c(3000,50000))+scale_y_log10()


p1<-ggplot(data = indf, aes(x=Var1, y=Freq))+geom_point()+ scale_y_log10()+ scale_x_log10()+xlab("Worker vs Capitalist Income")

intable<-(table(round(RevM[EmployM>0])))
indf<-data.frame(intable[2:length(intable)])
intable<-(intable[2:length(intable)])
intable<-1-(cumsum(intable)/sum(intable))
indf["Freq"]<-data.frame(intable)
var1<-sort(unique(round(as.numeric(RevM[EmployM>0]))))
indf["Var1"]<-var1[2:length(var1)]

p1+geom_point(data = indf, aes(x=Var1, y=Freq), colour="red")


##create worker income distribution graphs
dfWorkMon<-data.frame(RevM[EmployM>0])
colnames(dfWorkMon)<-"Worker_income"
m2<-mean((RevM[EmployM>0]))
sd2<-sd((RevM[EmployM>0]))
ggplot(data = dfWorkMon, aes(x=Worker_income))+geom_density()+stat_function(
  fun = dgamma,  args = list(.03, shape=2.7), col = "orange")+xlim(10,250)
mean(RevM[EmployM>=0])

##capitalist income distribution
dfWorkMon<-data.frame(RevM[EmployM<0])
colnames(dfWorkMon)<-"capitalist_income"
ggplot(data = dfWorkMon, aes(x=capitalist_income))+geom_density()+stat_function(
  fun = dpareto,  args = list(scale = 1000, shape=1.5), col = "orange")+xlim(1000,20000)
mean(RevM[EmployM>=0])


##create capitalist wealth distribution graphs
dfCapMon<-data.frame(RevM[EmployM<0])
colnames(dfCapMon)<-"Capitalist_Wealth"
ggplot(data = dfCapMon, aes(x=Capitalist_Wealth))+geom_histogram(breaks=seq(0, 4000, 
                                                                            by=1))+ scale_y_log10()+ scale_x_log10()

mean(RevM[EmployM<0])


##calculate histogram of labor and capital shares of wealth
WCratio<-(wageshare[300:t])/caprev[300:t]
WCRdf<-data.frame(WCratio)
colnames(WCRdf)<-"WageShare"
wcmean<-mean(WCratio[])
wcsd<-sd(WCratio)
ggplot(data = WCRdf, aes(x=WageShare))+geom_histogram(aes(y=stat(density)))+stat_function(fun = dnorm, 
                                                                                          args = list(mean = wcmean, sd = wcsd), col = "orange")
wcmean
wcsd

##Wage share of economy ratio over time
plot(300:t, WCratio, type = "l")

#calculate ratio of labor to capital share
Pratio<-((hisrev[300:t]-(hisvar[300:t])))/hisvar[300:t]
Pratio<-round(Pratio, digits = 3)
hist(Pratio, xlab = "Ratio of labor to capital share")

##create histogram for constant capital distribution historical 
hist(hiscon[300:t], xlab = "Total Constant Capital, historical")

##create histogram for constant capital distribution in economy
freqdf<-data.frame((ConcapM[EmployM<0]))
colnames(freqdf)<-"Constant_Capital"
maxr<-max(freqdf)
ggplot(data = freqdf, aes(x=Constant_Capital))+geom_histogram(colour="black", fill=NA)+ scale_y_log10()+ scale_x_log10()

##wage differential distribution
hist(WageLevM[WageLevM>0])
plot(300:t, FixexpendM[300:t], type = "l")

##create annualized distributions
aCon<-anConCap[anVarCap>0]/d
aVar<-anVarCap[anVarCap>0]
aRev<-anRev[anVarCap>0]
aLab<-anEmployeesFin[anVarCap>0]
aRoP<-(aRev-(aVar+aCon))/(aVar+aCon)
aCapIn<-aCon/aVar
aCapIn2<-aCon/aLab
plot(aCapIn, aRoP, log = "xy")
#plot(aLab, aCapIn, log = "xy")
abline(lm(aRoP ~ aCapIn), col = "blue")
summary(lm(aCapIn ~ aRoP))

