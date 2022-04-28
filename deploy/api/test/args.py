import json

args = {'region': "['US','AFRICA']"}

args = {'region': "['US']"}

args = {'region': "['US']", 'age': '10', 'sex': 'both', 'year': '1995'}



#json string data
args = json.loads('{"region":["US"], "age":10, "sex":"both", "year":1995}')


request = region=['US']&age=10&sex=both&year=1995