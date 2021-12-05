import numpy as np
import pandas as pd


# part 1
with open("day5/input.txt",'r') as fh:
    lines = fh.readlines()

lines = [line.rstrip().replace(" ","").split("->") for line in lines]
lines = pd.Series(lines)

def extract_xy (line):
    (start_x, start_y) =(int(s) for s in line[0].split(","))
    (end_x, end_y) =(int(s) for s in line[1].split(","))
    if (start_x == end_x):
        x = np.repeat(start_x, abs(start_y-end_y)+1)
        if (start_y <= end_y):
            y = np.arange(start_y, end_y+1, 1)
        else:
            y = np.arange(start_y, end_y-1, -1)
    elif (start_y == end_y):
        y = np.repeat(start_y, abs(start_x-end_x)+1)
        if (start_x <= end_x):
            x = np.arange(start_x, end_x+1, 1)
        else:
            x = np.arange(start_x, end_x-1, -1)
    else:
        x = pd.Series(dtype='int64')
        y = pd.Series(dtype='int64')
        
    return pd.DataFrame({"x":x, "y":y}, index = None)

xy_coords = lines.map(extract_xy)

xy_coords = pd.concat(xy_coords.tolist()).reset_index(drop=True)

xy_coords = xy_coords.groupby(['x','y']).size().reset_index(name="count")

sum(xy_coords['count'] > 1) # 5690

# part 2
def extract_xy_withDiag (line):
    (start_x, start_y) =(int(s) for s in line[0].split(","))
    (end_x, end_y) =(int(s) for s in line[1].split(","))
    if (start_x == end_x):
        x = np.repeat(start_x, abs(start_y-end_y)+1)
        if (start_y <= end_y):
            y = np.arange(start_y, end_y+1, 1)
        else:
            y = np.arange(start_y, end_y-1, -1)
    elif (start_y == end_y):
        y = np.repeat(start_y, abs(start_x-end_x)+1)
        if (start_x <= end_x):
            x = np.arange(start_x, end_x+1, 1)
        else:
            x = np.arange(start_x, end_x-1, -1)
    elif (abs(start_x-end_x) == abs(start_y-end_y)):
        if (start_x <= end_x):
            x = np.arange(start_x, end_x+1, 1)
        else:
            x = np.arange(start_x, end_x-1, -1)
        if (start_y <= end_y):
            y = np.arange(start_y, end_y+1, 1)
        else:
            y = np.arange(start_y, end_y-1, -1)
    else:
        x = pd.Series(dtype='int64')
        y = pd.Series(dtype='int64')
        
    return pd.DataFrame({"x":x, "y":y}, index = None)

xy_coords = lines.map(extract_xy_withDiag)

xy_coords = pd.concat(xy_coords.tolist()).reset_index(drop=True)

xy_coords = xy_coords.groupby(['x','y']).size().reset_index(name="count")

sum(xy_coords['count'] > 1) # 17741
