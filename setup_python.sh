#!/usr/bin/env bash
# Set up a Python virtual environment with SymPy
# for cross-validating group theory computations.
#
# Requires: uv (https://docs.astral.sh/uv/)
#
# Usage: ./setup_python.sh

set -euo pipefail

cd "$(dirname "$0")"

echo "Creating .venv with Python 3.11..."
uv venv .venv --python 3.11

echo "Installing packages..."
uv pip install --python .venv/bin/python \
    sympy \
    numpy

echo ""
echo "Verifying imports..."
.venv/bin/python -c "
import sympy;  print(f'  sympy {sympy.__version__}')
import numpy;  print(f'  numpy {numpy.__version__}')
from sympy.combinatorics import Permutation, SymmetricGroup, DihedralGroup
print('  sympy.combinatorics OK')
"

echo ""
echo "Done. Use .venv/bin/python to run scripts."
