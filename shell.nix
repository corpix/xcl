let
  inherit (pkgs)
    writeScript
    writeShellScriptBin
    writeText
    fetchFromGitHub
    symlinkJoin
  ;
  inherit (pkgs.stdenv)
    mkDerivation
  ;
  inherit (pkgs.lib)
    concatStringsSep
    attrByPath
  ;

  ##

  nixpkgs = <nixpkgs>;
  pkgs = import nixpkgs {};

  ##

  lisp = let
    scripts = let
      getenv = concatStringsSep " " [
        "#+sbcl sb-ext:posix-getenv"
        "#+clozure ccl:getenv"
      ];
    in {
      env = writeText "env.lisp" ''
        (require "asdf")
        (let* ((root (make-pathname :directory (${getenv} "PROJECT_ROOT"))))
          (push root asdf:*central-registry*))
      '';

      bootstrap = "${fetchFromGitHub {
        owner = "quicklisp";
        repo = "quicklisp-bootstrap";
        rev = "37cff6b7cf91a6260db2a4831d183d8ff696d91d";
        sha256 = "sha256-nJWAZILuYVcckjbDssEkFkL24sW5/NxcBa+KL8YpaXc=";
      }}/quicklisp.lisp";

      init = writeText "init.lisp" ''
        (load #p"${scripts.bootstrap}")

        (let* ((root (make-pathname :directory (${getenv} "PROJECT_ROOT")))
               (ql-root (merge-pathnames "vendor" root)))
          (when (not (probe-file ql-root))
            (quicklisp-quickstart:install :path ql-root)))
      '';

      slynk = writeText "slynk.lisp" ''
        (ql:quickload :slynk)
        (setf slynk::*loopback-interface* "127.0.0.1")
        (slynk:create-server :port 4005 :style :spawn)
        (loop while t do (sleep 1))
      '';
      swank = writeText "swank.lisp" ''
        (ql:quickload :swank)
        (setf swank::*loopback-interface* "127.0.0.1")
        (swank:create-server :port 4005 :style :spawn)
        (loop while t do (sleep 1))
      '';
    };

    mkLisp = name: let
      pkg = pkgs.${name};
      cli = {
        # different implementations support different CLI flags
        # this attrset is about to smooth the difference
        load = attrByPath [name] "--load" {};
        eval = attrByPath [name] "--eval" {};
      };

      wrapCommand = command: concatStringsSep " " [
        command
        ''${cli.load} "$PROJECT_ROOT/vendor/setup.lisp"''
        ''${cli.load} "${scripts.env}"''
      ];

      wrapper = writeShellScriptBin name ''
        set -e
        if [ -z "$PROJECT_ROOT" ]
        then
          echo "PROJECT_ROOT env variable required" 1>&2
          echo "It should point to the project VCS root" 1>&2
          exit 1
        fi

        if [ ! -f "$PROJECT_ROOT/vendor/setup.lisp" ]
        then
          ${pkg}/bin/${name} \
                           ${cli.load} "${scripts.init}" \
                           ${cli.eval} "(quit)"
        fi

        exec ${wrapCommand "${pkg}/bin/${name}"} "$@"
      '';
      slynk = writeShellScriptBin "${name}-slynk" ''
        set -e
        exec ${wrapCommand "${pkg}/bin/${name}"} ${cli.load} "${scripts.slynk}"
      '';
      swank = writeShellScriptBin "${name}-swank" ''
        set -e
        exec ${wrapCommand "${pkg}/bin/${name}"} ${cli.load} "${scripts.swank}"
      '';
    in symlinkJoin {
      name = "sbcl";
      paths = [
        wrapper
        slynk
        swank
        pkg
      ];
    };
  in {
    # NOTE: tried ecl & clisp... don't want to support them, too much headache

    sbcl = mkLisp "sbcl";
    ccl  = mkLisp "ccl";
  };

  ##

  shell = ''
    set -e
    export PROJECT_ROOT="$(pwd)"
    export LANG="en_US.UTF-8"
    export NIX_PATH="nixpkgs=${nixpkgs}"

    if [ ! -z "$PS1" ]
    then
      export SHELL="${pkgs.fish}/bin/fish"
      exec "$SHELL" --login --interactive
    fi
  '';

  ##

  packages = with pkgs.lispPackages; [
    quicklisp-to-nix
    quicklisp-to-nix-system-info
  ];

  env = mkDerivation rec {
    name = "shell";
    buildInputs = with pkgs; [
      glibcLocales bashInteractive
      nix cacert coreutils
      git
      rlwrap

      lisp.sbcl
      lisp.ccl
    ] ++ packages;
    shellHook = shell;
  };
in env
