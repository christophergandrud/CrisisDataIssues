############
# Comparative Statics Fiscal Costs/Contingent Liabilities Model
# Christopher Gandrud
# 8 April 2014
############

# Import packages
import random
import numpy as np
import math
import pandas 
import matplotlib.pyplot as plt

# Initialise lists
pi = []     # performance threshold
Cost = []   # fiscal costs
q1 = []     # immediate cost policies
q2 = []     # contingent policies
h = []      # discount factors

# Set fixed parameters
p = 0.2       # assume that 20 percent of contingent 
              # liabilities are realised
b = 0.3       # pre-contingent liability realisation benefit
R = 0.1       # non-pecuniary benefits

# Set performance threshold


# Find discount factors
## k is the degree of discounting
## D is the delay
def hypDiscount(k, D):
    Out = float(1)/(1 + k * D)
    return(Out)

# Create values
Dh = 2
kh = np.array(range(20))

for i in kh:
    h.append(hypDiscount(k = i, D = Dh))

plt.plot(kh, h, 'bo')
plt.show()


# Realised liabilities

