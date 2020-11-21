import matplotlib.pyplot as plt
import pandas as pd

df1 = pd.read_csv("./trace.ctf_out")

#df2 = pd.read_csv("./trace_no_hashtbl.ctf_out", names=['a', 'c'])

plt.plot(df1)

plt.show()
