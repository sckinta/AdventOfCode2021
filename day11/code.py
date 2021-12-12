#!/usr/bin/env python
# coding: utf-8

# In[122]:


import numpy as np


# In[123]:


with open("input.txt","r") as fh:
    lines = fh.readlines()
initial_m = [list(line.rstrip()) for line in lines]
initial_m = np.array(initial_m).astype(int)


# In[54]:


nr,nc = initial_m.shape


# In[115]:


m = initial_m
flashed_total = 0
for step in range(100):
    m = m + 1
    (index_r, index_c) = np.where(m > 9)
    flashed_indexes = [(index_r[x],index_c[x]) for x in np.arange(np.size(index_r))]
    flashed_indexes_total = flashed_indexes
    while (len(flashed_indexes) > 0):
        for i in np.arange(len(flashed_indexes)):
            neighbors_r = np.arange((flashed_indexes[i][0]-1),(flashed_indexes[i][0]+2))
            neighbors_c = np.arange((flashed_indexes[i][1]-1),(flashed_indexes[i][1]+2))
            neighbors_indexes_tmp = np.array([(r, c) for r in neighbors_r for c in neighbors_c if (r >=0 and r < nr and c >=0 and c < nc)])
            for j in np.arange(neighbors_indexes_tmp.shape[0]):
                m[neighbors_indexes_tmp[j,0],neighbors_indexes_tmp[j,1]] = m[neighbors_indexes_tmp[j,0],neighbors_indexes_tmp[j,1]] + 1
        (index_r, index_c) = np.where(m > 9)
        flashed_indexes_updated = [(index_r[t],index_c[t]) for t in np.arange(np.size(index_r))]        
        flashed_indexes = [index for index in flashed_indexes_updated if not(index in flashed_indexes_total)]
        flashed_indexes_total = flashed_indexes_updated
    m[np.where(m > 9)]= 0
    flashed_total += len(flashed_indexes_total)
flashed_total


# # part 2

# In[120]:


m = initial_m
step = 0
while(sum(sum(m)) > 0):
    step += 1
    m = m + 1
    (index_r, index_c) = np.where(m > 9)
    flashed_indexes = [(index_r[x],index_c[x]) for x in np.arange(np.size(index_r))]
    flashed_indexes_total = flashed_indexes
    while (len(flashed_indexes) > 0):
        for i in np.arange(len(flashed_indexes)):
            neighbors_r = np.arange((flashed_indexes[i][0]-1),(flashed_indexes[i][0]+2))
            neighbors_c = np.arange((flashed_indexes[i][1]-1),(flashed_indexes[i][1]+2))
            neighbors_indexes_tmp = [(r, c) for r in neighbors_r for c in neighbors_c if (r >=0 and r < nr and c >=0 and c < nc)]
            for j in np.arange(len(neighbors_indexes_tmp)):
                m[neighbors_indexes_tmp[j][0],neighbors_indexes_tmp[j][1]] = m[neighbors_indexes_tmp[j][0],neighbors_indexes_tmp[j][1]] + 1
        (index_r, index_c) = np.where(m > 9)
        flashed_indexes_updated = [(index_r[t],index_c[t]) for t in np.arange(np.size(index_r))]        
        flashed_indexes = [index for index in flashed_indexes_updated if not(index in flashed_indexes_total)]
        flashed_indexes_total = flashed_indexes_updated
    m[np.where(m > 9)]= 0

step

