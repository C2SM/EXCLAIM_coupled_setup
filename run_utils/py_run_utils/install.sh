#!/usr/bin/bash -l

VENV="/dev/shm/${USER}/.venv"
VENV_SQUASHFS="/dev/shm/${USER}/venv.squashfs"
rm -rf ${VENV}
mkdir -p ${VENV}
ln -s ${VENV} .venv

uv venv --relocatable --python="$(which python)"
source .venv/bin/activate
uv sync --no-cache --link-mode=copy --compile-bytecode --active --no-editable --inexact || exit
mksquashfs ${VENV} ${VENV_SQUASHFS} -no-recovery -noappend -Xcompression-level 3 || exit

rsync -av ${VENV_SQUASHFS} .
rm -rf ${VENV} ${VENV_SQUASHFS} .venv
