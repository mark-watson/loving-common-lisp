# Installation:

As of November 2024, the only way I can run this example on my M2 Mac is in conda:

install miniconda: https://docs.anaconda.com/miniconda/miniconda-other-installer-links/

export PATH=/Users/markw/bin/miniconda3/bin/:$PATH
conda install -c conda-forge spacy
python -m spacy download en_core_web_sm

cd PYTHON_SPACY_SETUP_install

pip install -e spacystub
cd ..


    pip install -r requirements.txt
    python -m pip install spacy
    python -m spacy download en_core_web_sm
    # pip install falcon

If you get an error installing that you need sudo permissions, try instead installing in user directory:

    python -m pip install -U spacy
