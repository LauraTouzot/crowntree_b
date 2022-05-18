# Code to fit Allometric data to Europe and North America data

Need to run with 

```
library(targets)
options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")
tar_option_set(packages = c("dplyr", "nlme", "lme4", "clustermq", "tidyr"))
tar_make_clustermq(workers = 4)  
```