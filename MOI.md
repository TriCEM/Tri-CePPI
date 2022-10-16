# Manuscript of Interest

Collection of manuscripts for Tri-CePPI model.  
**_TA_**: are take away points for consideration or useful insights.

## ID Models/Software


###[Shchur _et al._ 2022/VGsim](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010409)
**_TA_**:  Fast/efficient, somewhat flexible, compartment based simulation software: accounts for host population structure, pathogen evolution, host immunity, contact matrices.

##### Model Framework      
- Runs forward sims through a Gillespie-style algorithm in a SIS Compartment model framework (can vary S and I compartments to model waning immunity, vaccination, etc.).    
- code base is C++ w/ Python interface/wrapper

##### Coding Decisions
- Allows for host population (contact) structure.    
- Adaptive molecular evolution but only positive fitness effects.    
- Immunity is modeled as a Markovian (no accruing of immunity) 
- _Within_ population transmission depends on a contact density parameter --> NPIs are modeled through this contact density parameter 
- _Between_ population transmission is under a migration model 
	- From/To assymetric probability matrix 
		- Deal with extinction of demes by always having individuals return home/have short trips  
	- Cumulative upper bounds of migration --> assumes within >> between and authors note that it is suboptimal if a freely mixing population 
- Populations models as _distrete demes_ (within transmission above) with individuals traveling between demes by migration above 
   


###[Moshiri _et al._ 2018/FAVITES](https://academic.oup.com/bioinformatics/article/35/11/1852/5161084?login=false)
**_TA_**: Simulates the full end-to-end epidemic dataset (social contact network, transmission history, incomplete sampling, viral phylogeny, error-free sequences and real-world sequencing imperfections) - via a generative model = computationally expensive.     

- _NB_ his dissertation on FAVITES has additional [details](https://escholarship.org/uc/item/62s7q92d)


##### Model Framework    
- Agent based stochastic simulation 
	- truly modular framework allow for calling wide spectrum
-  Code base in Python with API for different modules.   
- File formats somewhat unique to program (multiple of them) 
	
##### Coding Decisions    
- Pays particular attention to contact network: uses `NetworkX` for various implementations 
- [General workflow](https://github.com/niemasd/FAVITES/wiki/General-Workflow) or [Fig1](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6931354/)
	- Initialization via "seeds" or individuals infected at t=0
		- For _seed_ step genetics, uses HMM to carry around realistic viral sequences (throughout? - comp expensive...)  
- Transmission seems to be on a scheduler versus loop? Says various models either assume exponential waiting times versus POMPs moving between states (Need to dig into modules to see how these differ)  
- Final set of steps involve making realistic sampling frameworks and phylogenetic trees (_i.e._ branching processes) 


**Considerations**

- Large number of dependencies, some of which are author's packages (eg. niemasd/Dual-Birth-Simulator, niemasd/GEMF in python vs C, TreeSwift)
		- _N.B._ many related to simulating sequences/seq error rates   
	
-  Notes the importance of epistasis for viral evolution/dynamics?  
-  Transmission network file format: Self-edges (i.e., same node in columns 1 and 2) denote removal of infection, either via recovery or death




###[Lequime _et al._ 2020/nosoi](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13422)
**_TA_**:  

##### Model Framework     
##### Coding Decisions  
	

###[Verity Unpublished/SIMPLEGEN](https://github.com/mrc-ide/SIMPLEGEN)
**_TA_**:  

##### Model Framework     
##### Coding Decisions  
	


## ID Dynamic Modeling

###[Blenkinsop _et al._ 2022](https://elifesciences.org/articles/76487)
TA: Clinical data of HIV VL and Tcell infxns to resolve the branch length/ancestral depth in the Coal Tree

###[Brockmann & Helbing 2013](https://www.science.org/doi/10.1126/science.1245200)

**_TA_**: Canonical flu spread via airlines/networks manuscript


## Genomics

## Epidemiology
