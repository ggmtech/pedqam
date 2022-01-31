import pandas as pd

df = pd.DataFrame( 
  {"Education"  : [5, 5, 7],
   "Experience" : [1, 3, 8],
   "Salary"    : [ 40000, 50000, 80000]
   }
   );
df
print(df)

# Convert Pandas DataFrame to NumPy array
np_array = df.to_numpy()
np_array
print(np_array)

###########
df = pd.DataFrame(
{"A" : ['a' ,'a', 'a', 'b', 'b' ,'b'],
"B" : ['A' ,'B', 'C', 'A', 'B' ,'C'],
"C" : [4, 5, 6 , 7 ,8 ,9]})

df
df.pivot( columns='B', values='C', index='A')

df=pd.DataFrame({'A': [4, 7], 'B': [5, 8], 'C': [6, 9]})
df
df.melt()

## pd.concat: Combine Data-Frames

df1 = pd.DataFrame(
{"A" : [1 ,2, 3],
"B" : [4, 5, 6],
"C" : [7, 8, 9]})
df2 = pd.DataFrame(
{"A" : [10 ,11],
"B" : [12, 13],
"C" : [14, 15]})
print(df1)
print(df2)
pd.concat([df1,df2])


# pd.explode: Transform each element of a list-like to a row
df=pd.DataFrame({'A':[[1,2,3],[4,5,6]]})
df
df.explode('A')

# Stack: Stack columns to index
df = pd.DataFrame([[0, 1], [2, 3]],
                                    index=['A', 'B'],
                                    columns=['COL1', 'COL2'])
df
df.stack()

# Unstack: Unstack columns from index
index = pd.MultiIndex.from_tuples([('A', 'col1'), ('A', 'col2'),
                                   ('B', 'col1'), ('B', 'col2')])
df = pd.Series(np.arange(1.0, 5.0), index=index)
df
df.unstack()


#######################################
# All data science projects begin with accessing the data and reading it correctly
import pandas as pd
print(pd.__version__)
# !pip install --upgrade pandas

# create json  file will be a very simple one:

[
    {
        "userId": 1,
        "firstName": "Jake",
        "lastName": "Taylor",
        "phoneNumber": "123456",
        "emailAddress": "john.smith@example.com"
    },
    {
        "userId": 2,
        "firstName": "Brandon",
        "lastName": "Glover",
        "phoneNumber": "123456",
        "emailAddress": "brandon.glover@example.com"
    }
]

# save it as sample.json in the same location as your Python code.

# And the second file will be a nested JSON file:

[
    {
        "userId": 1,
        "firstName": "Jake",
        "lastName": "Taylor",
        "phoneNumber": "123456",
        "emailAddress": "john.smith@example.com",
        "courses": {
            "course1": "mathematics",
            "course2": "physics",
            "course3": "engineering"
        }
    },
    {
        "userId": 2,
        "firstName": "Brandon",
        "lastName": "Glover",
        "phoneNumber": "123456",
        "emailAddress": "brandon.glover@example.com",
        "courses": {
            "course1": "english",
            "course2": "french",
            "course3": "sociology"
        }
    }
]

# Convert simple JSON to Pandas DataFrame in Python using pd.read_json() Pandas method. 
# It parses a JSON string and converts it to a Pandas DataFrame:

import pandas as pd
df = pd.read_json("sample.json")
print(df) # We get exactly the content of the JSON file converted to a DataFrame.

# nested
import pandas as pd
import json
with open('nested_sample.json','r') as f:
    data = json.loads(f.read())
df = pd.json_normalize(data)

# Let’s take a look at the JSON converted to DataFrame:

print(df)

###########################

import pandas as pd
!conda install altair
import altair as alt


# Fetch data
df_mortality = pd.read_csv(
    "https://www.mortality.org/Public/STMF/Outputs/stmf.csv",
    skiprows=2,
)

# Select countdf_mortalityf interest and only "both" sexes
# Note: Germany "DEUTNP" and "USA" have short time series
df_mortality = df_mortality[
    df_mortality["CountryCode"].isin(["CAN", "CHE", "FRATNP", "GBRTENW", "SWE"])
    & (df_mortality["Sex"] == "b")
].copy()

# Change to ISO-3166-1 ALPHA-3 codes
df_mortality["CountryCode"].replace(
    {"FRATNP": "FRA", "GBRTENW": "England & Wales"}, inplace=True
)

# Create population pro rata temporis (exposure) to ease aggregation
df_mortality = df_mortality.assign(
    population=lambda df: df["DTotal"] / df["RTotal"]
)

# Data aggregation per year and country
df_mortality = (
    df_mortality.groupby(["Year", "CountryCode"])[["population", "DTotal"]]
    .sum()
    .assign(CDR=lambda x: x["DTotal"] / x["population"])
    # .filter(items=["CDR"])  # make df even smaller
    .reset_index()
    .assign(Year=lambda x: pd.to_datetime(x["Year"], format="%Y"))
)

chart = (
    alt.Chart(df_mortality)
    .mark_line()
    .encode(
        x="Year:T",
        y=alt.Y("CDR:Q", scale=alt.Scale(zero=False)),
        color="CountryCode:N",
    )
    .properties(title="Crude Death Rate per Year")
    .interactive()
)
# chart.save("crude_death_rate.html")
chart


################
import numpy as np
import matplotlib.pyplot as plt


categories   = ['Food Quality', 'Food Variety', 'Service Quality', 'Ambiance', 'Affordability']
restaurant_1 = [4, 4, 5, 4, 3]
restaurant_2 = [5, 5, 4, 5, 2]
restaurant_3 = [3, 4, 5, 3, 5]

label_loc = np.linspace(start=0, stop=2 * np.pi, num=len(restaurant_1))

plt.figure(figsize=(8, 8))
plt.subplot(polar=True)
plt.plot(label_loc, restaurant_1, label='Restaurant 1')
plt.plot(label_loc, restaurant_2, label='Restaurant 2')
plt.plot(label_loc, restaurant_3, label='Restaurant 3')
plt.title('Restaurant comparison', size=20)
lines, labels = plt.thetagrids(np.degrees(label_loc), labels=categories)
plt.legend()
plt.show()

### corrected
categories = ['Food Quality', 'Food Variety', 'Service Quality', 'Ambiance', 'Affordability']
categories = [*categories, categories[0]]

restaurant_1 = [4, 4, 5, 4, 3]
restaurant_2 = [5, 5, 4, 5, 2]
restaurant_3 = [3, 4, 5, 3, 5]
restaurant_1 = [*restaurant_1, restaurant_1[0]]
restaurant_2 = [*restaurant_2, restaurant_2[0]]
restaurant_3 = [*restaurant_3, restaurant_3[0]]

label_loc = np.linspace(start=0, stop=2 * np.pi, num=len(restaurant_1))

plt.figure(figsize=(8, 8))
plt.subplot(polar=True)
plt.plot(label_loc, restaurant_1, label='Restaurant 1')
plt.plot(label_loc, restaurant_2, label='Restaurant 2')
plt.plot(label_loc, restaurant_3, label='Restaurant 3')
plt.title('Restaurant comparison', size=20, y=1.05)
lines, labels = plt.thetagrids(np.degrees(label_loc), labels=categories)
plt.legend()
plt.show()

############
# Radar Charts with Plotly

import plotly.graph_objects as go
import plotly.offline as pyo


categories = ['Food Quality', 'Food Variety', 'Service Quality', 'Ambience', 'Affordability']
categories = [*categories, categories[0]]

restaurant_1 = [4, 4, 5, 4, 3]
restaurant_2 = [5, 5, 4, 5, 2]
restaurant_3 = [3, 4, 5, 3, 5]
restaurant_1 = [*restaurant_1, restaurant_1[0]]
restaurant_2 = [*restaurant_2, restaurant_2[0]]
restaurant_3 = [*restaurant_3, restaurant_3[0]]


fig = go.Figure(
    data=[
        go.Scatterpolar(r=restaurant_1, theta=categories, name='Restaurant 1'),
        go.Scatterpolar(r=restaurant_2, theta=categories, name='Restaurant 2'),
        go.Scatterpolar(r=restaurant_3, theta=categories, name='Restaurant 3')
    ],
    layout=go.Layout(
        title=go.layout.Title(text='Restaurant comparison'),
        polar={'radialaxis': {'visible': True}},
        showlegend=True
    )
)

pyo.plot(fig)


## Image 3 – Radar chart with Plotly (image by author)
# And that’s all there is to it! Plotly also makes it easy to fill the polygons – just specify fill='toself'. Here’s an example:

import plotly.graph_objects as go
import plotly.offline as pyo


categories = ['Food Quality', 'Food Variety', 'Service Quality', 'Ambience', 'Affordability']
categories = [*categories, categories[0]]

restaurant_1 = [4, 4, 5, 4, 3]
restaurant_2 = [5, 5, 4, 5, 2]
restaurant_3 = [3, 4, 5, 3, 5]
restaurant_1 = [*restaurant_1, restaurant_1[0]]
restaurant_2 = [*restaurant_2, restaurant_2[0]]
restaurant_3 = [*restaurant_3, restaurant_3[0]]


fig = go.Figure(
    data=[
        go.Scatterpolar(r=restaurant_1, theta=categories, fill='toself', name='Restaurant 1'),
        go.Scatterpolar(r=restaurant_2, theta=categories, fill='toself', name='Restaurant 2'),
        go.Scatterpolar(r=restaurant_3, theta=categories, fill='toself', name='Restaurant 3')
    ],
    layout=go.Layout(
        title=go.layout.Title(text='Restaurant comparison'),
        polar={'radialaxis': {'visible': True}},
        showlegend=True
    )
)

pyo.plot(fig)








############################
##########################
#############################
import os
import shutil
import numpy as np
import pandas as pd
import calendar
from datetime import datetime
from fpdf import FPDF

import matplotlib.pyplot as plt
from matplotlib import rcParams
rcParams['axes.spines.top'] = False
rcParams['axes.spines.right'] = False
view raw


# You can use the calendar library to get the last day for any year/month combination. 
# Here’s the entire code snippet:

def generate_sales_data(month: int) -> pd.DataFrame:
    # Date range from first day of month until last
    # Use ```calendar.monthrange(year, month)``` to get the last date
    dates = pd.date_range(
        start=datetime(year=2020, month=month, day=1),
        end=datetime(year=2020, month=month, day=calendar.monthrange(2020, month)[1])
    )
    
    # Sales numbers as a random integer between 1000 and 2000
    sales = np.random.randint(low=1000, high=2000, size=len(dates))
    
    # Combine into a single dataframe
    return pd.DataFrame({
        'Date': dates,
        'ItemsSold': sales
    })

# Test
generate_sales_data(month=3)


# data visualization and an example call:

def plot(data: pd.DataFrame, filename: str) -> None:
    plt.figure(figsize=(12, 4))
    plt.grid(color='#F2F2F2', alpha=1, zorder=0)
    plt.plot(data['Date'], data['ItemsSold'], color='#087E8B', lw=3, zorder=5)
    plt.title(f'Sales 2020/{data["Date"].dt.month[0]}', fontsize=17)
    plt.xlabel('Period', fontsize=13)
    plt.xticks(fontsize=9)
    plt.ylabel('Number of items sold', fontsize=13)
    plt.yticks(fontsize=9)
    plt.savefig(filename, dpi=300, bbox_inches='tight', pad_inches=0)
    plt.close()
    return
              
# Test
december = generate_sales_data(month=12)

plot(data=december, filename='december.png')




##################
###################
import pandas as pd
import numpy as np
# set a random seed
np.random.seed(5)
# gender 60% male 40% female
# age from poisson distribution with lambda=25
# score a random integer from 0 to 100
df = pd.DataFrame({'gender':np.random.choice(a=['m','f'], size=20, p=[0.6,0.4]),
                   'age':np.random.poisson(lam=25, size=20),
                   'score_a':np.random.randint(100, size=20),
                   'score_b':np.random.randint(100, size=20),
                   'score_c':np.random.randint(100, size=20)})
df

# With the random.shuffle() we can shuffle randomly the numpy arrays.
# set a random seed
np.random.seed(5)
arr = df.values
np.random.shuffle(arr)
arr




####################
####################
# whois 
#!pip install python-whois
import whois
domain='www.pyshark.com'
domain_info = whois.whois(domain)


# Note that this code will only execute successfully if the domain name is registered else error.
# fn to check
def check_reg(name):
    try:
        domain_info = whois.whois(name)
        return True
    except:
        return False
check_reg(domain)

domain_info = whois.whois(domain)
for key in domain_info.keys():
    print(key)
# The final step is to print out the key-value pairs to have the actual information about our domain:

for key, value in domain_info.items():
    print(key,':', value)


###############
###############
#To connect to Python by a trusted connection using the following syntax:

import pypyodbc as odbc
import pandas as pd
import numpy as np
driver = 'Driver={ODBC Driver 17 for SQL Server};'
server = 'Server=localhost;Database=master;Trusted_Connection=True;'
print(driver + server)
conn = odbc.connect('Driver={ODBC Driver 17 for SQL Server};'
                      'Server=localhost;'
                      'Database=master;'
                     'Trusted_Connection=yes;')
print(conn)
conn = odbc.connect("Driver={ODBC Driver 17 for SQL Server};Server=localhost;Database=master;"                  "uid=garyhutson;pwd=password")
df = pd.read_sql_query('SELECT * FROM [master].[dbo].[RockStars]', conn)
print(df.head(20))
print(df.describe())



##################### googlesheets
###################
# ! pip install gsheets
# Then you have to log in to Google Developers Console. 
# Create (or select) a project and enable the Drive API and Sheets API under Google Apps APIs. 
# Go to the Credentials for your project and 
# create New credentials > OAuth client ID > of type Desktop app. 
# You now can download the client secret by pressing the download button. 
# Save it to your working directory and rename it as client_secrets.json.



import pandas as pd
import numpy as np
from gsheets import Sheets

sheets = Sheets.from_files('client_secrets.json', '~/storage.json')

url = 'url_of_google_sheet'
s = sheets.get(url)

#the first method is by saving the data into CSV files
#the number here represents the first and the second tab of the sheet
s.sheets[0].to_csv(make_filename="sheet1.csv")
s.sheets[1].to_csv(make_filename="sheet2.csv")
pd.read_csv('sheet1.csv')
#the second method is by reeading the values directly with pandas 
pd.DataFrame(s.sheets[0].values())


##############
# QR codes

# pip install pyqrcode
# pip install pypng
import pyqrcode
dest = 'https://pyshark.com/generate-qr-code-using-python/'
myQR = QRCode(dest)

# we create an instance of .QRCode() class and pass our dest (destination) 
# to it as an argument and in return it creates a QR code object.

myQR.show()


# To reuse this QR code, we will go ahead and save it as PNG:

myQR.png('qrcode1.png', scale=8)  
# Note: scale=8 is a parameter that adjusts the size of the QR code PNG, 


# QR Code Object Parameters
myQR = QRCode(dest)
myQR = QRCode(dest, error='H', version=None, mode=None, encoding='iso-8859-1')
# content: this is the ‘target’ destination that we want to encode in the QR code.
# error: error correction level of the code (by default set to ‘H’ which is the highest possible level).
# version: specifies the size and data capacity of the code (can take integer values between 1 and 40) and when left unspecified, it will find the smallest possible QR code version to store the data that we want (knowing its size).
# mode: specifies how the content will be encoded (there are four options: numeric, alphanumeric, binary, kanji). If left unspecified, it will be guessed by the algorithm.
# encoding: specifies how the content will be encoded and defaults to iso-8.
# Detailed explanation of each parameter is available here.

# To test this, let’s try creating QR codes for: URL, address, and phone number:

dest = ['https://pyshark.com/generate-qr-code-using-python/',
        '1 Yonge Street, Toronto, Ontario, Canada',
        '+1 (999) 999-9999']
for i in dest:
    myQR = pyqrcode.QRCode(i)
    myQR.png('myqrcode'+str(dest.index(i))+'.png', scale=8)

# This will create and save three QR codes in the same directory where your script is located. For instance, each of these, when scanned, will be identified by an iPhone’s QR code decoder and apps to open them will be suggested automatically:

