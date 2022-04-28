import json

args = {'region': "['US','AFRICA']"}

args = {'region': "['US']"}

args = {'region': "['US']", 'age': '10', 'sex': 'both', 'year': '1995'}



table='cod'

# works
args = dict({"region":"'US'"})
args = dict({"age":"10","region":"['US']","sex":"both","year":"2019"})

# broken
args = dict({"age":"10","region":"'US'","sex":"both","year":"2019"})

