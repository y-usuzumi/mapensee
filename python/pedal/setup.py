#!/usr/bin/env python

try:
    # Use setuptools if available, for install_requires (among other things).
    import setuptools
    from setuptools import setup
except ImportError:
    setuptools = None
    from distutils.core import setup

version = "0.1"

kwargs = {}

with open("README.rst") as f:
    kwargs['long_description'] = f.read()

setup(
    name='Pedal',
    version=version,
    description='The Pedal language',
    author="Savor d'Isavano",
    author_email='anohigisavay@gmail.com',
    url='http://github.com/KenetJervet/mapensee',
    packages=["pedal", "pedal.test"],
    **kwargs
)
