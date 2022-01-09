#!/usr/bin/env python3

# File part of fa-scripts
# Copyright 2022 Notkea
# Licensed under the EUPL version 1.2

from _utils import get, qualify
from sys import argv
from dataclasses import dataclass, asdict
from json import dumps
from markdownify import markdownify

@dataclass(frozen=True)
class SubmissionMetadata:
  url: str
  download_url: str
  title: str
  author: str
  date: str
  tags: list[str]
  folder_names: list[str]
  description: str
  inline_writing_md: str

def extract_folders_links(page):
  folders_container = page.find(class_='folder-list-container')
  if folders_container is None: return []
  return folders_container.find_all('span')

def extract_submission_metadata(page, url):
  meta_container = page.find(class_='submission-id-sub-container')
  tags_container = page.find(class_='tags-row').find_all('a')
  return SubmissionMetadata(
    url=url,
    download_url=qualify(page.find(class_='download').find('a')['href']),
    title=meta_container.find(class_='submission-title').find('p').text,
    author=meta_container.find('a').find('strong').text,
    date=meta_container.find(class_='popup_date')['title'],
    tags=[ a.text for a in tags_container ],
    folder_names=[ s.text for s in extract_folders_links(page) ],
    description=str(page.find(class_='submission-description')),
    inline_writing_md=extract_raw_inline_writing_md(page),
  )

def extract_raw_inline_writing_md(page):
  # Use FA's rendered BBCode and convert it back to MD
  writing_container = page.find(class_='submission-writing')
  if writing_container is None: return None
  preview = writing_container.find('center').find('div')
  paragraphs = preview.contents[8:]  # skip file info
  if len(paragraphs) <= 1: return None  # no preview, unsupported file type
  return ''.join(str(p) for p in paragraphs)

def sanitise_text(raw):
  return raw.strip().replace('\xa0', ' ')  # NBSP

def to_markdown(raw):
  return markdownify(sanitise_text(raw), strip=['img', 'a'])

def fetch_submission_metadata(url):
  page = get(url)
  metadata = extract_submission_metadata(page, url)
  json_metadata = dumps(asdict(metadata), indent=2, sort_keys=True)
  print(json_metadata)

if __name__ == "__main__":
  if len(argv) != 2 or argv[1] in ('-h', '--help'):
    print('Fetches and prints a JSON object containing the metadata of a submission.')
    print('Usage: get-submission https://www.furaffinity.net/view/SOME_SUBMISSION/')
    exit(1)

  fetch_submission_metadata(argv[1])
