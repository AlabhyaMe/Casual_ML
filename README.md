## Causal Machine Learning
Impact evaluation methods such as Randomized Control Trials (RCTs), A/B testing, and Difference-in-Differences are powerful tools for causal inference, but they are not designed for prediction. Machine learning methods like Logistic Regression, Random Forest, and Neural Networks excel at prediction but fall short of providing causal insights.

### Files
Uplift Data.R is a R file that I used to generate the data. You can run this in your console.

Model Uplift.R is the R file that I used for data analysis. My article in Medium, Casual Machine Learning-- Using ML Models in Social Experiments, uses the analytics done in this file. 

Uplift Model Logistic.ipynb is the Python file. While the approach is similar to R, some figures vary by small margins due to the different approaches the console takes for initialization. The main results remain the same.   

Vocational.csv is the dataset

### Libraries used
#### For R
library(tidyverse)

library(readxl)

library(tidymodels)

library(VIP)

library(cowplot)

library(ROCR)

library(gridExtra)

#### For Python
import polars as pl

from sklearn.model_selection import train_test_split

from sklearn.linear_model import LogisticRegression

import matplotlib.pyplot as plt

import numpy as np

import pandas as pd

from scipy import stats

from sklearn.metrics import accuracy_score

Note: I use Polars instead of Pandas for data analytics in Python. It is much more efficient and the coding style is more consistent and similar to R.

### Methodology 

**Randomized Control Trial**

Effect of Treatment = E( P = 1| T = 1) — E( P = 1 | T = 0)

**Logistic Regression**

lr = (Placement ~ c + age + score + gender + experience + previous_exp + distance_majorcity + owns_motor+treatment)

**Uplift Model**

(Placement ~ c + age + score + gender + experience + previous_exp + distance_majorcity + owns_motor|treatment=1) 
                                          -
(Placement ~ c + age + score + gender + experience + previous_exp + distance_majorcity + owns_motor|treatment=0)
