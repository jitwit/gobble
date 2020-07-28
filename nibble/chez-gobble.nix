{ stdenv, chez, chez-hemlock, chez-srfi, chez-euler, chez-matchable }:

stdenv.mkDerivation rec {
  pname = "chez-gobble";
  version = "0.1";
  src = ./../cobble;

  buildInputs = [ chez chez-hemlock chez-srfi chez-matchable ];

  libdirs = "${chez-hemlock}/lib/csv9.5-site/:${chez-euler}/lib/csv9.5-site/:${chez-srfi}/lib/csv9.5-site/:${chez-matchable}/lib/csv9.5-site/";
  libpath = "$out/lib/csv9.5-site/:${libdirs}";

  buildPhase = ''
    export CHEZSCHEMELIBDIRS=${libdirs}
    make prefix=$out CHEZ=${chez}/bin/scheme
  '';

  
  installPhase = ''
    mkdir -p $out/lib/csv9.5-site/
    mkdir -p $out/share/
    mkdir -p $out/bin/
    cp *.so $out/lib/csv9.5-site/
    cp share/trie.fasl $out/share/
    cp share/definitions.txt $out/share/
    echo "#!${chez}/bin/scheme --script" >> $out/bin/gobbler
    echo "(library-directories \"${libpath}\")" >> $out/bin/gobbler
    cat gobbler.ss >> $out/bin/gobbler
    chmod 755 $out/bin/gobbler
  '';

  meta = {
    description = "This package solves and generates boggle boards";
    homepage = https://github.com/jitwit/gobble/;
    maintainers = [ stdenv.lib.maintainers.jitwit ];
    license = stdenv.lib.licenses.gpl3;
  };

}
