import matplotlib.pyplot as plt
import pandas as pd

df1 = pd.read_csv("./b1") #pause at start of worker thread
df2 = pd.read_csv("./b2_no_pause") #no pause

#with auto_yields
df3 = pd.read_csv("./b3")
df4 = pd.read_csv("./b4")

df = pd.concat([df1, df2])


df.plot(kind="line")

plt.show()
