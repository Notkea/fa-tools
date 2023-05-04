# File part of fa-scripts
# Copyright 2023 Notkea
# Licensed under the EUPL version 1.2

from _utils import qualify
from dataclasses import dataclass

@dataclass(frozen=True)
class SubmissionMetadata:
  url: str
  download_url: str
  title: str
  author: str
  date: str
  tags: list[str]
  folder_names: list[str]
  description_html: str
  inline_writing_html: str

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
    description_html=str(page.find(class_='submission-description')),
    inline_writing_html=extract_raw_inline_writing_html(page),
  )

def extract_folders_links(page):
  folders_container = page.find(class_='folder-list-container')
  if folders_container is None: return []
  return folders_container.find_all('span')

def extract_tags(page):
  tags_container = page.find(class_='tags-row')
  if tags_container is None: return []
  return [ a.text for a in tags_container.find_all('a') ]

def extract_raw_inline_writing_html(page):
  # Use FA's rendered BBCode and convert it back to MD
  writing_container = page.find(class_='submission-writing')
  if writing_container is None: return None
  preview = writing_container.find('center').find('div')
  paragraphs = preview.contents[8:]  # skip file info
  if len(paragraphs) <= 1: return None  # no preview, unsupported file type
  return ''.join(str(p) for p in paragraphs)
