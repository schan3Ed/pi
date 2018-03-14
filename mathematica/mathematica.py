import pandas as pd
import csv

df1 = pd.read_csv('singlegrid_100-1.csv', header=None, names=['piHat'])
df2 = pd.read_csv('singlegrid_100-2.csv', header=None, names=['piHat'])
df3 = pd.read_csv('singlegrid_100-3.csv', header=None, names=['piHat'])
df4 = pd.read_csv('singlegrid_100-4.csv', header=None, names=['piHat'])
df5 = pd.read_csv('singlegrid_100-5.csv', header=None, names=['piHat'])

BKV = 3.141592653589793238462643383

# def writeTable(df1, df2, df3, df4, df5):
# 	df1['numThrows'] = 10
# 	df2['numThrows'] = 100
# 	df3['numThrows'] = 1000
# 	df4['numThrows'] = 10000
# 	df5['numThrows'] = 100000
# 	df = df1.append(df2, ignore_index=True)
# 	df = df.append(df3, ignore_index=True)
# 	df = df.append(df4, ignore_index=True)
# 	df = df.append(df5, ignore_index=True)
#     for i in range(len(df)):
#     	df['sampleId'] = i
#     df['error'] = abs(df['piHat']-BKV)
#     return df

df1['numThrows'] = 10
df2['numThrows'] = 100
df3['numThrows'] = 1000
df4['numThrows'] = 10000
df5['numThrows'] = 100000
df = df1.append(df2, ignore_index=True)
df = df.append(df3, ignore_index=True)
df = df.append(df4, ignore_index=True)
df = df.append(df5, ignore_index=True)
for i in range(len(df)):
    df.loc[i,'sampleId'] = i+1
df['error'] = abs(df['piHat']-BKV)
df = df[['sampleId', 'numThrows', 'piHat', 'error']]
#print writeTable(df1,df2,df3,df4,df5)

#writeTable(df1,df2,df3,df4,df5).to_csv('fg_asym_pi_plain_needles1_math_5.csv', sep=' ', index=False, header=True)

df.to_csv('fg_asym_pi_plain_needles1_math_5.csv', index=False, header=True)

csv_file = raw_input('fg_asym_pi_plain_needles1_math_5.csv')
txt_file = raw_input('fg_asym_pi_plain_needles1_math_5.txt')
with open(txt_file, "w") as my_output_file:
    with open(csv_file, "r") as my_input_file:
        [ my_output_file.write(" ".join(row)+'\n') for row in csv.reader(my_input_file)]
    my_output_file.close()