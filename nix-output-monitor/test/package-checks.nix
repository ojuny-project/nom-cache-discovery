{
  pkgs,
  lib,
  src,
}:
let
  captured = pkgs.haskellPackages.callPackage (src + "/nix-output-monitor") {
    mkDerivation = args: args;
  };
  dependencyNames =
    dependencies:
    map (dependency: dependency.pname or (lib.getName dependency)) (
      builtins.filter (dependency: dependency != null) dependencies
    );
  hasDependency = section: name: builtins.elem name (dependencyNames captured.${section});
  requireDependencies =
    section: names:
    let
      missing = builtins.filter (name: !(hasDependency section name)) names;
    in
    if missing == [ ] then
      ""
    else
      ''
        echo "missing from ${section}: ${lib.concatStringsSep " " missing}" >&2
        exit 1
      '';
in
{
  # INVENTORY: package.3.2
  package-3-2 = pkgs.runCommand "nom-package-3-2" { } ''
    ${requireDependencies "executableHaskellDepends" [
      "http-client"
      "http-client-tls"
    ]}
    touch $out
  '';

  # INVENTORY: package.3.3
  package-3-3 = pkgs.runCommand "nom-package-3-3" { } ''
    ${requireDependencies "testHaskellDepends" [
      "http-client"
      "http-client-tls"
    ]}
    touch $out
  '';

  # INVENTORY: package.3.5
  package-3-5 =
    pkgs.runCommand "nom-package-3-5"
      {
        nativeBuildInputs = [
          (lib.getBin pkgs.haskellPackages.cabal2nix)
          pkgs.python3
        ];
        PYTHONDONTWRITEBYTECODE = "1";
      }
      ''
        mkdir -p work
        cp -r ${src}/nix-output-monitor work/nix-output-monitor
        cd work
        cabal2nix nix-output-monitor > generated.nix
        python3 ${./package-dep-diff.py} generated.nix ${src}/nix-output-monitor/default.nix
        touch $out
      '';
}
