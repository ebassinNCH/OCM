import pandas as pd
import numpy as np
import os
from time import ctime
import sys
sys.path.append('c:/code/general')
from NCHGeneral import *

os.chdir('c:/AdvAnalytics/OCM/CCSI/QtrAggregate/Working')
df = Use('dfbene')
listBenes = pd.unique(df.BeneSK).tolist()
print(len(listBenes))
