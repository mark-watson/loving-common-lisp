from distutils.core import setup

setup(name='spacy_nlp_server',
      version='0.11',
      packages=['spacynlpserver', 'spacynlpserver.test'],
      license='Apache 2',
      scripts=['bin/spacynlpserver'],
      long_description=open('README.md').read())
