import matplotlib.pyplot as plt

import pandas as pd

df = pd.read_csv("./visit_level", names=['a', 'b'])

df.set_index('a',inplace=True)

ax = df['b'].plot(color="green", label='visited', style=".")

plt.xlabel("Time")
plt.ylabel("Depth of object in the graph")
plt.legend()

plt.show()
