# Purpose
This sandbox repo is to explore implementing our own generated contact matrix as the first step in the FAVITES model.

## Quickstart
Download docker, python3. Make sure docker file is executable or call python3. Make config file. Call script: `python3 FAVITES/run_favites_docker.py --config my_contact_config.json`


## Coding Pragmatics 
In FAVITES can use the _ContactNetworkGenerator_File_ option:  `"ContactNetworkGenerator":       "File",` in the config file for _ContactNetworkGenerator_ module 
- _NB_ must the [FAVITES network format](https://github.com/niemasd/FAVITES/wiki/File-Formats#contact-network-file-format):
```
#NODE<TAB>label<TAB>attributes (csv or .)
#EDGE<TAB>u<TAB>v<TAB>attributes (csv or .)<TAB>(d)irected or (u)ndirected

NODE<TAB>Bill<TAB>USA,Mexico
NODE<TAB>Eric<TAB>USA
NODE<TAB>Curt<TAB>.
EDGE<TAB>Bill<TAB>Eric<TAB>.<TAB>d
EDGE<TAB>Curt<TAB>Eric<TAB>Friends<TAB>u
```


# Structure of this Repo



