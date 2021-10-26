#!/usr/bin/env python

from setuptools import setup, find_packages

setup(name='cargo-lock-utils',
      version='0.0.1',
      packages=find_packages(),
      scripts=[
        'compute-checksum',
        'list-cargo-lock',
        'merge-cargo-lock',
      ],
     )
