#!/usr/bin/env python

import subprocess
import sys
import os


def get_cargo_flags(debug=False):
    flags = ["--manifest-path=backend/Cargo.toml", "--verbose", "--target-dir=target"]
    if not debug:
        flags.append("--release")
    return flags


def build(debug=False):
    print(">>> Building backend. This requires Rust toolchain.")
    cargo_flags = get_cargo_flags(debug=debug)
    subprocess.check_call(
        [
            "cargo",
            "build",
        ]
        + cargo_flags
    )

    print(">>> Copying backend assets.")
    if os.name == "nt":
        pattern = "%s.dll"
    else:
        pattern = "lib%s.so"
    for name in ["notebackend", "notebackend_python"]:
        basename = pattern % name
        dst = os.path.join("frontend", basename)
        src = os.path.join("..", "target", debug and "debug" or "release", basename)
        if not os.path.exists(dst):
            os.symlink(src, dst)
        else:
            if os.readlink(dst) != src:
                os.unlink(dst)
                os.symlink(src, dst)

    print(">>> Building frontend. This requires Lazarus toolchain.")
    lazbuild_flags = ["--build-mode=%s" % (debug and "Debug" or "Release")]
    if sys.platform == "linux":
        lazbuild_flags.append("--ws=qt6")
    subprocess.check_call(
        [
            "lazbuild",
            *lazbuild_flags,
            "frontend/foonote.lpi",
        ]
    )


def test(debug):
    print(">>> Running backend tests.")
    cargo_flags = get_cargo_flags(debug=debug)
    ret = subprocess.check_call(
        [
            "cargo",
            "test",
        ]
        + cargo_flags
    )


def run(args):
    if os.name == "nt":
        pattern = "%s.exe"
    else:
        pattern = "./%s"
    basename = pattern % "foonote"
    env = os.environ.copy()
    if sys.platform == "linux":
        env["LD_LIBRARY_PATH"] = "."
    subprocess.check_call([basename] + args, cwd="frontend", env=env)


def help():
    if os.name == "nt":
        pattern = "%s"
    else:
        pattern = "./%s"

    name = pattern % "do.py"
    print(
        """Usage:

    %(name)9s build       -  build (release)
    %(name)9s debug build -  build (debug)
    %(name)9s test        -  run tests
    %(name)9s build test  -  build and test
    %(name)9s run [args]  -  run program with args

Runtime environment variables:

    FOONOTE_LOG=debug     - enable debug logging
    FOONOTE_LOG=trace,frontend=debug
                          - enable trace logging, but only debug logging for frontend
"""
        % {"name": name}
    )


def main(args=None):
    if not args or any(s in args for s in ["help", "--help", "-h", "/?"]):
        return help()
    try:
        debug = False
        if any(s in args for s in ["dbg", "debug", "d"]):
            debug = True
        if "build" in args or "b" in args:
            build(debug=debug)
        if "test" in args or "t" in args:
            test(debug=debug)
        if "run" in args:
            arg_index = args.index("run")
            run_args = args[arg_index + 1 :]
            run(run_args)
    except subprocess.CalledProcessError as ex:
        print("Failed. Exit code: %s" % ex.returncode)
        sys.exit(ex.returncode)


if __name__ == "__main__":
    main(sys.argv[1:])
