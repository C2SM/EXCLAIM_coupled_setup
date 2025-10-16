#!/usr/bin/bash -l
#
pushd ../run_utils/py_run_utils 2>&1 >/dev/null || exit
uenv run --view default icon/25.2:v3 -- $PWD/install.sh
popd 2>&1 >/dev/null || exit
mv ../run_utils/py_run_utils/venv.squashfs .
rm -rf .venv
mkdir .venv
