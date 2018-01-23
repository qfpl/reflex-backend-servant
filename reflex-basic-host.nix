let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-basic-host = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-basic-host";
      rev = "66428fafc3fb5e9735ceb00bcdeab4c42ea59edc";
      sha256 = "0i94qzn56kd2mxgxnghrjq5qbq7g5nsjrjfcb1hkfs6kgfmgw1yc";
    };
  };
in
  sources.reflex-basic-host

