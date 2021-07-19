#!/usr/bin/env python3

# File part of fa-scripts
# Copyright 2021 Notkea
# Licensed under the EUPL version 1.2

from _utils import *
from sys import argv, stdout
from dataclasses import dataclass
from collections import deque
from csv import DictWriter

@dataclass(frozen=True)
class FolderListing:
  name: str
  url: str

@dataclass(frozen=True)
class SubmissionEntry:
  title: str
  rating: str
  type: str
  url: str
  thumbnail_url: str
  folder_name: str
  folder_url: str

def extract_other_folders(page):
  folder_container = page.find(class_='user-folders')
  if folder_container is None: return []
  links = folder_container.find_all(class_='dotted')
  return [ FolderListing(name=l.text, url=qualify(l['href'])) for l in links ]

def extract_submission(figure, folder):
  rating, type = figure['class']
  caption_link = figure.find('figcaption').find('a')
  return SubmissionEntry(
    title=caption_link['title'],
    rating=rating,
    type=type,
    url=qualify(caption_link['href']),
    thumbnail_url=qualify(figure.find('img')['src']),
    folder_name=folder.name,
    folder_url=folder.url,
  )

def extract_submissions(page, folder):
  figures = page.find(id='gallery-gallery').find_all('figure')
  return [ extract_submission(f, folder) for f in figures ]

def extract_next_link(page):
  next_button = page.find(class_='submission-list').find('button', text='Next')
  if next_button is None: return None
  return qualify(next_button.find_parent('form')['action'])

def extract_all_submissions(main_gallery_page):
  listing_queue = deque([ FolderListing('Main Gallery', main_gallery_page) ])
  folders_discovered = False

  while listing_queue:
    folder_listing = listing_queue.popleft()
    page = get(folder_listing.url)
    yield from extract_submissions(page, folder_listing)

    next_link = extract_next_link(page)
    if next_link:
      listing_queue.appendleft(FolderListing(folder_listing.name, next_link))

    if not folders_discovered:
      listing_queue.extend(extract_other_folders(page))
      folders_discovered = True

if __name__ == "__main__":
  if len(argv) != 2 or argv[1] in ('-h', '--help'):
    print('List all the submissions in the given gallery as CSV.')
    print('Usage: list-submissions https://www.furaffinity.net/gallery/SOME_USER/')
    exit(1)

  submissions = extract_all_submissions(argv[1])
  writer = DictWriter(stdout, SubmissionEntry.__dataclass_fields__.keys())
  writer.writeheader()
  writer.writerows(s.__dict__ for s in submissions)
