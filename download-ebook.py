#!/usr/bin/env python3

# File part of fa-scripts
# Copyright 2023 Notkea
# Licensed under the EUPL version 1.2

from _utils import *
from _submission import SubmissionMetadata, extract_submission_metadata
from sys import argv
from markdownify import markdownify

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

def sanitise_text(raw):
  return raw.strip().replace('\xa0', ' ')  # NBSP

def to_markdown(raw):
  return markdownify(sanitise_text(raw), strip=['img', 'a'])

def submission_to_markdown(url):
  page = get(url)

  metadata = extract_submission_metadata(page, url)
  print(format_pandoc_markdown_headers(metadata))

  content = metadata.inline_writing_html or metadata.description_html
  print(to_markdown(content))

if __name__ == "__main__":
  if len(argv) != 2 or argv[1] in ('-h', '--help'):
    print('Downloads and generate a pandoc markdown file from a text submission.')
    print('Usage: download-ebook https://www.furaffinity.net/view/SOME_SUBMISSION/')
    exit(1)

  submission_to_markdown(argv[1])
