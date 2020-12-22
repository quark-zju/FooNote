import os
from pathlib import Path
from PIL import Image

import subprocess

for path in ["/usr/bin/inkscape", "/Program Files/Inkscape/inkscape.exe"]:
    if os.path.exists(path):
        os.environ["PATH"] = (
            os.path.dirname(path) + os.pathsep + (os.environ["PATH"] or ".")
        )

# Export svg to bitmaps of different sizes
src = "icon.svg"
pngs = []
for size in [16, 32, 48, 256]:
    dst = "%s.png" % size
    print("Exporting %s from %s" % (dst, src))
    cmd = [
        "inkscape",
        "-o",
        dst,
        "-w",
        str(size),
        "-h",
        str(size),
        src,
    ]
    subprocess.check_call(cmd)
    pngs.append(dst)

# https://stackoverflow.com/a/58770704/1187066
def bake_several_pngs_to_ico(sourcefiles, targetfile):
    """Converts several PNG files into one ICO file.

    args:
        sourcefiles (list of str): A list of pathnames of PNG files.
        targetfile (str): Pathname of the resulting ICO file.

    Use this function if you want to have fine-grained control over
    the resulting icon file, providing each possible icon resolution
    individually.

    Example::

        sourcefiles = [
            "Path/to/logo_16x16.png",
            "Path/to/logo_32x32.png",
            "Path/to/logo_48x48.png"
        ]
        targetfile = "Path/to/logo.ico"
        bake_several_pngs_to_ico(sourcefiles, targetfile)
    """

    # Write the global header
    number_of_sources = len(sourcefiles)
    data = bytes((0, 0, 1, 0, number_of_sources, 0))
    offset = 6 + number_of_sources * 16

    # Write the header entries for each individual image
    for sourcefile in sourcefiles:
        img = Image.open(sourcefile)
        data += bytes(
            (
                img.width % 256,  # 0: 256px
                img.height % 256,
                0,
                0,
                1,
                0,
                32,
                0,
            )
        )
        bytesize = Path(sourcefile).stat().st_size
        data += bytesize.to_bytes(4, byteorder="little")
        data += offset.to_bytes(4, byteorder="little")
        offset += bytesize

    # Write the individual image data
    for sourcefile in sourcefiles:
        data += Path(sourcefile).read_bytes()

    # Save the icon file
    Path(targetfile).write_bytes(data)


dst = "mainicon.ico"
print("Building %s from %r" % (dst, pngs))
bake_several_pngs_to_ico(pngs, dst)
