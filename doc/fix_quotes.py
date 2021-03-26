import glob

LEFT = "“"
RIGHT = "”"
QUOTES = [LEFT, RIGHT]

for name in glob.glob("*.rst"):
    with open(name, "r") as f:
        text = f.read()
    newtext = ""
    state = 0
    for ch in text:
        if ch == LEFT or ch == RIGHT:
            newtext += QUOTES[state]
            state = 1 - state
        else:
            newtext += ch
    if newtext != text:
        print("Fixing %s" % name)
        with open(name, "wb") as f:
            f.write(newtext.encode("utf-8"))
