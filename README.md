### depression_longitudinal

##### Main question: Can we predict depression from clinical data collected 5 years before. 

Outcome: depression at time T2:

1. **Depression_mod_above_at_phq** (n=95, mean=0.10, min=0 ,  max=1)    
2. **PHQ9_Sum** (mean = 3.47, min = 0, max = 24). 
above 10 is defined as clinically depression. the data is skewed 
 
---

Covariates:

1.	smry_psychosis [Non-PS,PS]
2.	smry_man [0,1]                 
3.	**smry_dep [0,1]**                
4.	smry_gad [0,1]                  
5.	smry_sep [0,1]                   
6.	smry_phb [0,1]                  
7.	smry_soc [0,1]                   
8.	smry_pan [0,1]                  
9.	smry_agr [0,1]                   
10.	smry_ocd [0,1]                  
11.	smry_add [0,1]  
12.	smry_odd [0,1]  
13.	smry_con [0,1]  

In first step we will use only smry_dep (n=36).
In next step all the above or XOR of all of them will be added. 
Last step is the p general factor score (z score for the level of the psychosis of the child). 
No need for the entire clinical bucket in this research.  

Beside the above we will add to the model: 

1. sex
2. age
3. duration (t1 till t2)
3. race
4. Avg Parent Education

* siblings – in next step as sensitivity analysis
* Depression FH - in next step           

---

Predictors: 

1. all cognitive data. We will start with the summary features per test (14 total) and then switch to the raw. 

2. Within individual variability:
    1. exeWivAArZv2     (multi tasking)
    2. exeWivSArZv2   
    3. epimemWivAArZv2  (memory)        
    4. epimemWivSArZv2          
    5. cpxresWivAArZv2  (thinking flexibility; Ability to abstract)   
    6. cpxresWivSArZv2              
    7. soccogWivAArZv2  (identify emotions, faces, age)    
    8. soccogWivSArZv2             
    9. wivAArZv2           
    10. wivSArZv2

---
We are using SPLS regression that exhibits good performance when:
1. the sample size is much smaller than the total number of variables
2. the covariates are highly correlated. 

* predictors in form of matrix. 
* The responses and the predictors are assumed to be numerical and should not contain missing values.
* As part of pre-processing, the predictors are centered and scaled and the responses are centered
automatically as default by the package ‘spls’.
* SPLS regression has two main tuning parameters:
  1. eta - a sparsity tuning parameter [0,1]
  2. K - the number of hidden (latent) components. [1, min {p,(v − 1)n/v}]


---
second step:

1. check each domain contribution. 
the domains were selected according to Tyler's factor analysis:
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4345134/
as the data includes both accuracy and speed, the domains were choosen according to the Confirmatory bifactor model of the CNB Efficiency scores.
in total 6 domains:
    1. memory [40]  
    2. social cognition [63]
    3. executive [37]
    4. complex cognition [24]

    5. motor [11]
    6. iq [3]

2. check sex influence  
 
 

