import matplotlib.pyplot as plt
import pandas as pd
import sys

dbv = pd.read_csv(sys.argv[1])
plt.plot(dbv, label='block validator')

plt.xlabel("Blocks validated")
plt.ylabel("Seconds")
plt.legend()

plt.savefig('blocks_validated.png')

plt.clf()

dbv = pd.read_csv(sys.argv[2])
plt.plot(dbv, label='objects added')

plt.xlabel("Blocks validated")
plt.ylabel("Objects added")
plt.legend()

plt.savefig('obj_added.png')
