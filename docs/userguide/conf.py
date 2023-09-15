project = 'WRF-Hydro Modeling System'
author = 'WRF-Hydro Team'
copyright = '2023, '+author
version = 'v5.3.0'
release = '5.3.0'
try:
    import sphinx_rtd_theme
    html_theme_path = sphinx_rtd_theme.get_html_theme_path()
    html_theme = 'sphinx_rtd_theme'
except:
    pass
#these are enforced by rstdoc, but keep them for sphinx-build
numfig = 0
smartquotes = 0
source_suffix = '.rest'
templates_path = []
language = None
highlight_language = "none"
default_role = 'math'
pygments_style = 'sphinx'
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']
master_doc = 'index'