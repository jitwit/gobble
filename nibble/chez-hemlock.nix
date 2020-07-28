{ stdenv, fetchgit, chez }:

stdenv.mkDerivation {
  pname = "chez-hemlock";
  version = "1.0";

  src = fetchgit {
    url = "https://github.com/jitwit/hemlock.git";
    rev = "26f7652c566d3444718f14f54ee3f65c27a30434";
    sha256 = "1z5n2c5y7q2jhp9abgxxkara3wcg6whamk5kj77rlv2b15lic680";
  };

  buildInputs = [ chez ];

  buildPhase = "make chez=${chez}/bin/scheme";

  installPhase = "make install out=$out/lib/csv9.5-site";

  meta = {
    description = "Trees and other structures";
    homepage = https://github.com/jitwit/chez-hemlock/;
    maintainers = [ stdenv.lib.maintainers.jitwit ];
    license = stdenv.lib.licenses.asl20;
  };

}
