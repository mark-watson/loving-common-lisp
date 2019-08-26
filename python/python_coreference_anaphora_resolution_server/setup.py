from distutils.core import setup

setup(name='coref_server',
      version='0.11',
      packages=['corefserver', 'corefserver.test'],
      license='Apache 2',
      scripts=['bin/corefserver'],
      long_description=open('README.md').read())
