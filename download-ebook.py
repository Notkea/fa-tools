#!/usr/bin/env python3

# File part of fa-scripts
# Copyright 2023 Notkea
# Licensed under the EUPL version 1.2

from _utils import *
from sys import argv
from dataclasses import dataclass
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

def extract_folders_links(page):
  folders_container = page.find(class_='folder-list-container')
  if folders_container is None: return []
  return folders_container.find_all('span')

def extract_tags(page):
  tags_container = page.find(class_='tags-row')
  if tags_container is None: return []
  return [ a.text for a in tags_container.find_all('a') ]

def extract_submission_metadata(page, url):
  meta_container = page.find(class_='submission-id-sub-container')
  return SubmissionMetadata(
    url=url,
    download_url=qualify(page.find(class_='download').find('a')['href']),
    title=meta_container.find(class_='submission-title').find('p').text,
    author=meta_container.find('a').find('strong').text,
    date=meta_container.find(class_='popup_date')['title'],
    tags=extract_tags(page),
    folder_names=[ s.text for s in extract_folders_links(page) ],
  )

def format_list(lst):
  return ', '.join(lst) if lst else '_none_'

def format_pandoc_markdown_headers(sub_meta):
  return '\n'.join([
    f'% {sub_meta.title}',
    f'% {sub_meta.author}',
    f'% {sub_meta.date}',
    '',
    f'FurAffinity: {sub_meta.url}\n',
    f'Folders: {format_list(sub_meta.folder_names)}\n',
    f'Tags: {format_list(sub_meta.tags)}\n',
    '',
    '---',
    '',
  ])

def extract_raw_description(page):
  return str(page.find(class_='submission-description'))

def extract_raw_inline_writing(page):
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

def submission_to_markdown(url):
  page = get(url)

  metadata = extract_submission_metadata(page, url)
  print(format_pandoc_markdown_headers(metadata))

  content = extract_raw_inline_writing(page) or extract_raw_description(page)
  print(to_markdown(content))

if __name__ == "__main__":
  if len(argv) != 2 or argv[1] in ('-h', '--help'):
    print('Downloads and generate a pandoc markdown file from a text submission.')
    print('Usage: download-ebook https://www.furaffinity.net/view/SOME_SUBMISSION/')
    exit(1)

  submission_to_markdown(argv[1])
