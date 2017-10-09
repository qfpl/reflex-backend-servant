let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-basic-host = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-basic-host";
      rev = "5960a216d28c382ce40ff303a51e2823ddd63d5e";
      sha256 = "1j5ibs17740rxmvmqk0ycfhy2dii07w09l79n1qviw8bc2inkllr";
    };
  };
in
  sources.reflex-basic-host

