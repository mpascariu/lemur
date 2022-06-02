# lemur: Life Expectancy Monitor API (v1)

> Query data on life expectancy and causes of mortality for age-sex demographic groups in every country globally for specific years. 

## Endpoints

**life-expectancy.org/api/v1/cause_of_death**  
Returns numbers of deaths resulting from each cause of death.

**life-expectancy.org/api/v1/life_table**  
Returns life table vital rates (fertility, mortality) for each age and sex group.

**life-expectancy.org/api/v1/sdg**  
Returns numbers of deaths resulting from each cause of death relevant to sustainable development goals (SDGs).

**life-expectancy.org/api/v1/regions**  
Returns the complete list of regions (i.e. countries) that can be queried using the other API endpoints.


## Arguments

The 'regions' endpoint does not require any arguments.  

The other endpoints will all accept the arguments described below.  

Argument | Description
|:-- |:-----------
region | Region or country names as a list enclosed in square brackets (e.g. ['US'] or ['US', 'AFRICA']). Use the 'regions' endpoint for a complete list of acceptable values.
year | Accepts only one of the following values: 1990, 1995, 2000, 2010, 2015, 2019
age | Accepts only one of the following values: 0, 1, 5, 10, 15, ... , 100, 105, 110
sex | Accepts only of the following values: 'female', 'male', 'both'

## API Response
The API will return a json response with five elements:

Element | Description 
|:-- |:----------- 
status | http status code
message | Message describing outcome of operation writing to the database
timestamp | Date and time of response
data | Data resulting from query in json format

For example:
```{python}
{
  "status": 200,
  "message": "OK: Data successfully selected from database.",
  "timestamp": "2022-01-03 18:30:26+00",
  "data": '{"data":"{\"region\":{\"0\":\"BOLIVIA\",\"1\":\"BRUNEI\",...'
}
```


## Examples

**Query data using a url:**  
```{python}
http://life-expectancy.org/api/v1/cause_of_death?region=['US','AFRICA']&sex=both
```

**Query data from Python:**
```{python}
# import packages
import requests
import pandas as pd

# query arguments
args = {
  "region": "['US','AFRICA']",
  "year": 2019,
  "age": 10,
  "sex": "both"
  }

# submit query as GET request
response = requests.get(url = 'http://life-expectancy.org/api/v1/cause_of_death', 
                        params = args)

# format response as dictionary
response = response.json()

# check status
print(response.get('status'))
print(response.get('message'))

# extract data as pandas dataframe
if response.get('status') == 200:
  data = pd.DataFrame(json.loads(response.get('data')))
```

**Query data from R:**
```{r}
# import packages
library('httr')
library('jsonlite')
options(scipen = 999)

# query arguments
args <- list(region = "['US','AFRICA']",
             year = 2019,
             age = 10,
             sex = 'both')

# submit query as GET request
response <- httr::GET(url = 'http://life-expectancy.org/', 
                      path = 'api/v1/cause_of_death',
                      query = args)

# format response as list
response <- jsonlite::fromJSON(httr::content(response, as='text'))

# check status
print(response$status)
print(response$message)

# extract data in various formats
if(response$status == 200){
  
  # json string
  data <- response$data
  
  # json -> list of lists
  data <- jsonlite::fromJSON(data)
  
  # list of lists -> data.frame with cells containing lists
  # note: this is a convenient format for dealing with JSONs for some data.frame cells in R.
  data <- as.data.frame(do.call(cbind, data))
  
}
```





