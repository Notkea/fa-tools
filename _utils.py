# File part of fa-scripts
# Copyright 2021 Notkea
# Licensed under the EUPL version 1.2

from os import environ

COOLDOWN_SEC = float(environ.get('FA_COOLDOWN_SEC', 1))
REQUEST_HEADERS = dict([
  l.split(': ', 1)
  for l in environ['FA_REQUEST_HEADERS'].splitlines()
  if ': ' in l
])

BASE_URL = 'https://www.furaffinity.net'

import requests as req
from ratelimiter import RateLimiter
from bs4 import BeautifulSoup as bs

@RateLimiter(max_calls=1, period=COOLDOWN_SEC)
def get(url):
  # prevent the headers from leaking elsewhere
  if not url.startswith(BASE_URL):
    raise ValueError(f'This client only works for {BASE_URL}')

  response = req.get(url, headers=REQUEST_HEADERS)
  return bs(response.content, 'html5lib')  # html.parser is not lenient enough


from urllib.parse import urljoin

def qualify(path, base=BASE_URL):
  if path.startswith('//'): return 'https:' + path
  if '://' not in path: return urljoin(base, path)
  return path
