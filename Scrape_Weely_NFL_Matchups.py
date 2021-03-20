import requests
from bs4 import beautifulsoup4

# Make a request
url = 'http://www.vegasinsider.com/nfl/odds/las-vegas/'
res = requests.get(url)
res.raise_for_status()
soup = bs4.BeautifulSoup(res.text, 'html.parser')