with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "my-project";
  # If you need libraries, list them here
  buildInputs = [ zlib lazarus fpc automake xorg.libX11 atk cairo gtk2 gtk2-x11 glib gdk_pixbuf pango glibc ];
}
