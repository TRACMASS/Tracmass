# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
#sys.path.insert(0, os.path.abspath('../src'))

# -- Project information -----------------------------------------------------

project = 'TRACMASS'
copyright = '2019, Aitor Aldama Campino'
author = 'Aitor Aldama Campino'
version = '7.0.1'

# The full version, including alpha/beta/rc tags
release = '7.0.1-rc'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.intersphinx',
    'sphinx.ext.doctest',
    'sphinx.ext.coverage',
    'sphinx.ext.githubpages',
    'sphinx.ext.napoleon',
    'sphinx.ext.imgmath',
    'sphinx.ext.mathjax',
    'sphinx.ext.todo',
    'sphinxfortran.fortran_domain',
    'sphinxfortran.fortran_autodoc',
    ]


# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# Logo of TRACMASS
html_theme_options = {
'logo_only': False
}

# Logo
html_logo = 'figs/tracmasslogo.png'
## -- Options for Sphinx-Fortran ---------------------------------------------
# List of possible extensions in the case of a directory listing
fortran_ext = ['f90', 'F90', 'f95', 'F95']

# This variable must be set with file pattern, like "*.f90", or a list of them. 
# It is also possible to specify a directory name; in this case, all files than 
# have an extension matching those define by the config variable `fortran_ext` 
# are used.
fortran_src = [ os.path.abspath('../src/'),  ]

# Indentation string or length (default 4). If it is an integer, 
# indicates the number of spaces.
fortran_indent = 4



