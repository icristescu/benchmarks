
import matplotlib.pyplot as plt

import pandas as pd

plt.title('index#freeze\n irmin#less_pause\n dune exec -- ./bench/irmin-pack/layers.exe -b 10 --depth 20 --stats', fontsize=7)


dbv = pd.read_csv("./commit")
plt.plot(dbv, label='commits')

#to plot another commit file
#dbv = pd.read_csv("./commit_1")
#plt.plot(dbv, label='commits 1')


plt.xlabel("Commits")
plt.ylabel("Completed time (s)")
plt.legend()

plt.show()
