from distutils.core import setup

project_name = 'f90nml'
project_version = __import__(project_name).__version__
project_readme_fname = 'README.rst'

with open(project_readme_fname) as f:
    project_readme = f.read()

setup(
    name = project_name,
    version = project_version,
    description = 'Fortran 90 namelist parser',
    long_description = project_readme,
    author = 'Marshall Ward',
    author_email = 'python@marshallward.org',
    url = 'http://github.com/marshallward/f90nml',

    py_modules = ['f90nml'],

    classifiers = [
        'Development Status :: 3 - Alpha',
        'License :: OSI Approved :: Apache Software License',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.3',
        'Topic :: Utilities',
    ]
)
