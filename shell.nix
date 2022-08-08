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
  ;

  ##

  nixpkgs = <nixpkgs>;
  pkgs = import nixpkgs {};

  ##

  lisp = {
    env = writeText "env.lisp" ''
      (require "asdf")
      (let* ((root (sb-ext:posix-getenv "PROJECT_ROOT"))
             (root-abs (car (directory root))))
        (push root-abs asdf:*central-registry*))
    '';

    bootstrap = "${fetchFromGitHub {
      owner = "quicklisp";
      repo = "quicklisp-bootstrap";
      rev = "37cff6b7cf91a6260db2a4831d183d8ff696d91d";
      sha256 = "sha256-nJWAZILuYVcckjbDssEkFkL24sW5/NxcBa+KL8YpaXc=";
    }}/quicklisp.lisp";

    init = writeText "init.lisp" ''
      (load #p"${lisp.bootstrap}")

      (let* ((root (sb-ext:posix-getenv "PROJECT_ROOT"))
             (root-abs (car (directory root)))
             (ql-root-abs (merge-pathnames "vendor" root-abs)))
        (when (not (probe-file ql-root-abs))
          (quicklisp-quickstart:install :path ql-root-abs)))
    '';

    slynk = writeText "slynk.lisp" ''
      (ql:quickload :slynk)
      (setf slynk::*loopback-interface* "127.0.0.1")
      (slynk:create-server :port 4005 :style :spawn)
      (loop while t do (sleep 1))
    '';
  };

  ##

  packages = with pkgs.lispPackages; [
    quicklisp-to-nix
    quicklisp-to-nix-system-info
  ];

  sbcl-wrapped = let
    wrapCommand = command: concatStringsSep " " [
      command
      ''--load "$PROJECT_ROOT/vendor/setup.lisp"''
      ''--load "${lisp.env}"''
    ];

    sbcl-wrapper = writeShellScriptBin "sbcl" ''
      set -e
      if [ -z "$PROJECT_ROOT" ]
      then
        echo "PROJECT_ROOT env variable required" 1>&2
        echo "It should point to the project VCS root" 1>&2
        exit 1
      fi

      if [ ! -f "$PROJECT_ROOT/vendor/setup.lisp" ]
      then
        ${sbcl}/bin/sbcl \
                         --load "${lisp.init}" \
                         --eval "(quit)"
      fi

      exec ${wrapCommand "${sbcl}/bin/sbcl"} "$@"
    '';
    sbcl-slynk = writeShellScriptBin "sbcl-slynk" ''
      set -e
      exec ${wrapCommand "${sbcl}/bin/sbcl"} --load "${lisp.slynk}"
    '';

    sbcl = pkgs.sbcl;
  in symlinkJoin {
    name = "sbcl";
    paths = [
      sbcl-wrapper
      sbcl-slynk
      sbcl
    ];
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

  env = mkDerivation rec {
    name = "shell";
    buildInputs = with pkgs; [
      glibcLocales bashInteractive
      nix cacert coreutils
      git
      rlwrap
      sbcl-wrapped
    ] ++ packages;
    shellHook = shell;
  };
in env
