# Extract selected icons from elementary-icons at $PREFIX.
# Convert from svg to png.

import os

names = '''
actions/list-add
actions/open-menu
actions/window-close
actions/mail-send
actions/edit-copy
actions/edit-paste
apps/accessories-text-editor
devices/drive-harddisk-solidstate
mimes/office-database
mimes/package
mimes/text
mimes/text-x-preview
places/folder
places/folder-documents
places/folder-remote
places/network-workgroup
places/user-trash
places/user-bookmarks
status/locked
'''

prefix = os.getenv("PREFIX") or ''

for size in [16, 24, 32, 48, 64, 128]:
    for name in names.strip().split():
        path = prefix + name.replace('/', '/%s/' % size) + '.svg'
        if os.path.exists(path):
            out = '%s-%s.png' % (size, name.split('/')[-1])
            if not os.path.exists(out):
                os.system('inkscape %s --export-png=%s' % (path, out))
                os.system('optipng %s' % out)
        else:
            print('%s: not exists' % path)
