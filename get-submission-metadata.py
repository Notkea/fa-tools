#!/usr/bin/env python3

# File part of fa-scripts
# Copyright 2023 Notkea
# Licensed under the EUPL version 1.2

from _utils import get
from _submission import SubmissionMetadata, extract_submission_metadata
from sys import argv
from dataclasses import asdict
from json import dumps

def fetch_submission_metadata(url):
  page = get(url)
  metadata = extract_submission_metadata(page, url)
  json_metadata = dumps(asdict(metadata), indent=2, sort_keys=True)
  print(json_metadata)

if __name__ == "__main__":
  if len(argv) != 2 or argv[1] in ('-h', '--help'):
    print('Fetches and prints a JSON object containing the metadata of a submission.')
    print('Usage: get-submission-metadata https://www.furaffinity.net/view/SOME_SUBMISSION/')
    exit(1)

  fetch_submission_metadata(argv[1])
